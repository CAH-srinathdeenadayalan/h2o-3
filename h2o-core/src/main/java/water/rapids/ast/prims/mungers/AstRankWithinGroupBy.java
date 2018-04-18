package water.rapids.ast.prims.mungers;

import water.Iced;
import water.MRTask;
import water.fvec.Chunk;
import water.fvec.Frame;
import water.fvec.Vec;
import water.rapids.Env;
import water.rapids.ast.AstParameter;
import water.rapids.ast.AstPrimitive;
import water.rapids.ast.AstRoot;
import water.rapids.ast.params.AstNum;
import water.rapids.ast.params.AstNumList;
import water.rapids.vals.ValFrame;
import water.util.IcedHashMap;

import java.util.ArrayList;
import java.util.Arrays;


/** Given a dataframe, a list of groupby columns, a list of sort columns, a list of sort directions, a string
 * for the new name of the rank column, this class
 * will sort the whole dataframe according to the columns and sort directions.  It will add the rank of the
 * row within the groupby groups based on the sorted order determined by the sort columns and sort directions.  Note
 * that rank starts with 1.
 *
 * If there is any NAs in the sorting columns, the rank of that row will be NA as well.
 *
 * If there is any NAs in the groupby columns, they will be counted as a group and will be given a rank.  The user
 * can choose to ignore the ranks of groupby groups with NAs in them.
 */
public class AstRankWithinGroupBy extends AstPrimitive {

  @Override public String[] args() {
    return new String[]{"frame", "groupby_cols", "sort_cols", "sort_orders", "new_colname"};
  }

  @Override public String str(){ return "rank_within_groupby";}
  @Override public int nargs() { return 1+5; } // (rank_within_groupby frame groupby_cols sort_cols sort_orders new_colname)

  @Override public ValFrame apply(Env env, Env.StackHelp stk, AstRoot asts[]) {
    Frame fr = stk.track(asts[1].exec(env)).getFrame(); // first argument is dataframe
  /*  int[] groupbycols = ((AstNumList) asts[2]).expand4();  // groupby columns
    int[] sortcols =((AstNumList) asts[3]).expand4();  // sort columns */
    int[] groupbycols = ((AstParameter)asts[2]).columns(fr.names());
    int[] sortcols =((AstParameter)asts[3]).columns(fr.names());  // sort columns

    int[] sortAsc;
    if (asts[4] instanceof AstNumList)
      sortAsc = ((AstNumList) asts[4]).expand4();
    else
      sortAsc = new int[]{(int) ((AstNum) asts[4]).getNum()};  // R client can send 1 element for some reason
    String newcolname = asts[5].str();
    
    assert sortAsc.length==sortcols.length;
    SortnGrouby sortgroupbyrank = new SortnGrouby(fr, groupbycols, sortcols, sortAsc, newcolname);
    sortgroupbyrank.doAll(sortgroupbyrank._groupedSortedOut);  // sort and add rank column
    RankGroups rankgroups = new RankGroups(sortgroupbyrank._groupedSortedOut, sortgroupbyrank._groupbyCols,
            sortgroupbyrank._chunkStasG, sortgroupbyrank._newRankCol).doAll(sortgroupbyrank._groupedSortedOut);
    return new ValFrame(rankgroups._finalResult);
  }

  public class RankGroups extends MRTask<RankGroups> {
    final int _newRankCol;
    final int _groupbyLen;
    final int[] _groupbyCols;
    final IcedHashMap<GInfoPC, String>[] _chunkStasG;  // store all groupby class per chunk
    Frame _finalResult;

    private RankGroups(Frame inputFrame, int[] groupbycols, IcedHashMap<GInfoPC, String>[] chunkStas, int newRankCol) {
      _newRankCol = newRankCol;
      _groupbyCols = groupbycols;
      _groupbyLen = groupbycols.length;
      _chunkStasG = chunkStas; // store starting rank for next chunk
      _finalResult = inputFrame;
    }
    @Override
    public void map(Chunk[] chunks) {
      ArrayList<double[]> keys = new ArrayList<>();
      ArrayList<Long> currentRank = new ArrayList<>(); // keep track of rank
      int cidx = chunks[0].cidx();  // get current chunk id
      setStartRank(keys, currentRank, cidx);  // store correct starting rank for each group in this chunk
      double[] rowKey = new double[_groupbyLen];
      long crank=0;

      for (int rind=0; rind < chunks[0]._len; rind++) {
        if (!Double.isNaN(chunks[_newRankCol].atd(rind))) { // only rank when sorting columns contains no NAs
          for (int cind = 0; cind < _groupbyLen; cind++) {  // generate row key
            rowKey[cind] = chunks[_groupbyCols[cind]].atd(rind);
          }
          int keyIndex = findKeyIndex(keys,rowKey);
          if (keyIndex < 0) { // key not found before, add it here and set crank to 0
            crank = 1;
            keys.add(Arrays.copyOf(rowKey, _groupbyLen));
            currentRank.add(2l); // +1 for next row
          } else {
            crank = currentRank.get(keyIndex);
            currentRank.set(keyIndex, crank+1); // update the correct rank value for next value of the group
          }
          chunks[_newRankCol].set(rind, crank);
        }
      }
    }

    public void setStartRank(ArrayList<double[]> keys, ArrayList<Long> vals, int cidx) {
      if ((cidx >0) && (_chunkStasG[cidx-1] != null)) {
        IcedHashMap<GInfoPC, String> temp = _chunkStasG[cidx-1];
        for (GInfoPC key: temp.keySet()) {
          keys.add(key._gs);
          vals.add(key._val+1);
        }
      }
    }
  }

  public class SortnGrouby extends MRTask<SortnGrouby> {
    final int[] _sortCols;
    final int[] _groupbyCols;
    final int[] _sortOrders;
    final String _newColname;
    Frame _groupedSortedOut;  // store final result
    IcedHashMap<GInfoPC, String>[] _chunkStasG;  // store all groupby class per chunk
    final int _groupbyLen;
    final int _newRankCol;
    final int _numChunks;

    private SortnGrouby(Frame original, int[] groupbycols, int[] sortCols, int[] sortasc, String newcolname) {
      _sortCols = sortCols;
      _groupbyCols = groupbycols;
      _sortOrders = sortasc;
      _newColname = newcolname;
      _groupedSortedOut = original.sort(_sortCols, _sortOrders); // sort frame
      Vec newrank = original.anyVec().makeCon(Double.NaN);
      _groupedSortedOut.add(_newColname, newrank);  // add new rank column of invalid rank, NAs
      _numChunks = _groupedSortedOut.vec(0).nChunks();
      _chunkStasG = new IcedHashMap[_numChunks];
      _groupbyLen = _groupbyCols.length;
      _newRankCol = _groupedSortedOut.numCols() - 1;
    }


    @Override
    public void map(Chunk[] chunks) {
      int cidx = chunks[0].cidx();  // grab chunk id
      int chunkLen = chunks[0].len();
      _chunkStasG[cidx] = new IcedHashMap<>();
      GInfoPC gWork = new GInfoPC(_groupbyLen, 0);
      GInfoPC gOld = null; // use for reference
      for (int rind = 0; rind < chunkLen; rind++) { // go through each row and groups them
        boolean foundNA = false;
        for (int colInd = 0; colInd < _sortCols.length; colInd++) { // check sort columns for NAs
          if (Double.isNaN(chunks[_sortCols[colInd]].atd(rind))) {
            foundNA = true; //need to ignore this row
            break;
          }
        }

        if (!foundNA) { // no NA in sort columns
          chunks[_newRankCol].set(rind, 0); // set new rank to 0 from NA
          gWork.fill(rind, chunks, _groupbyCols);
          if (_chunkStasG[cidx].putIfAbsent(gWork, "") == null) { // Insert if not absent (note: no race, no need for atomic)
            gWork = new GInfoPC(_groupbyLen,1);   // need entirely new G
          } else {
            gOld = _chunkStasG[cidx].getk(gWork);     // Else get existing group
            gOld._val = gOld._val+1;                  // update the val count of it
          }
        }
      }
    }

    @Override
    public void reduce(SortnGrouby git) {  // copy over the information from one chunk to the final
      int numChunks = git._chunkStasG.length;  // total number of chunks existed
      GInfoPC gWork = new GInfoPC(_groupbyLen, 0);
      for (int ind = 0; ind < numChunks; ind++) {
        if (_chunkStasG[ind] == null) {
          if (git._chunkStasG[ind] != null && git._chunkStasG[ind].size() > 0) {
            _chunkStasG[ind] = new IcedHashMap<>();

            for (GInfoPC key : git._chunkStasG[ind].keySet()) {
              gWork.copyKey(key._gs, key._val);
              if (_chunkStasG[ind].putIfAbsent(gWork, "") == null) { // Insert if not absent (note: no race, no need for atomic)
                gWork = new GInfoPC(_groupbyLen,1);   // need entirely new G
              }
            }
          }
        }
      }
    }

    @Override
    public void postGlobal() {  // change counts per group per chunk to be cumulative
      ArrayList<double[]> keys = new ArrayList<>(); // store all keys encountered so far
      ArrayList<Long> cumCount = new ArrayList<>();

      for (int cInd = 0; cInd < _numChunks; cInd++) {
        IcedHashMap<GInfoPC, String> tempG = _chunkStasG[cInd];
        if (tempG != null) {
          for (GInfoPC key : tempG.keySet()) { // go through all keysets in current chunk
            int kindex = findKeyIndex(keys, key._gs);
            if (kindex >= 0) {
              long val = cumCount.get(kindex) + key._val;
              cumCount.set(kindex, val);
              GInfoPC gwork = tempG.getk(key);
              gwork._val = val;
            } else {  // new key to the ArrayLists
              keys.add(key._gs);
              cumCount.add(key._val);
            }
          }
        }
        GInfoPC gwork = new GInfoPC(_groupbyLen, 0);
        int currentArraySize = keys.size(); // found keys in previous chunks but not this chunk, need to add them
        for (int ind=0; ind<currentArraySize;ind++) { // go through all keys in Arraylist so far
          double[] akey = keys.get(ind);
          gwork.copyKey(akey, cumCount.get(ind));

          if (tempG.putIfAbsent(gwork, "")==null) { // new element inserted
            gwork = new GInfoPC(_groupbyLen,0);
          }
        }
      }
    }
  }

  public static int findKeyIndex(ArrayList<double[]> tempMap, double[] currKey) {
    if (tempMap == null || tempMap.size()==0)
      return -1;

    int arraySize = tempMap.size();
    for (int aIndex = 0; aIndex < arraySize; aIndex++)
      if (Arrays.equals(tempMap.get(aIndex), currKey))
        return aIndex;
    return -1;
  }

  /**
   * Store rank info for each chunk.
   */
  public class GInfoPC extends Iced {
    public final double[] _gs;  // Group Key: Array is final; contents change with the "fill"
    int _hash;
    long _val; // store count of the groupby key inside the chunk

    public GInfoPC(int ncols, int val) {
      _gs = new double[ncols];  // denote a groupby group
      _val = val;               //number of rows belonging to the groupby group
    }

    public GInfoPC fill(int row, Chunk chks[], int cols[]) {
      for (int c = 0; c < cols.length; c++) {// For all selection cols
        _gs[c] = chks[cols[c]].atd(row); // Load into working array
      }

      _val = 1;
      _hash = hash();
      return this;
    }

    public GInfoPC copyKey(double chks[], long val) {
      for (int c = 0; c < chks.length; c++) // For all selection cols
        _gs[c] = chks[c]; // Load into working array
      _hash = hash();
      _val = val;
      return this;
    }

    protected int hash() {
      long h = 0;                 // hash is sum of field bits
      for (double d : _gs) h += Double.doubleToRawLongBits(d);
      // Doubles are lousy hashes; mix up the bits some
      h ^= (h >>> 20) ^ (h >>> 12);
      h ^= (h >>> 7) ^ (h >>> 4);
      return (int) ((h ^ (h >> 32)) & 0x7FFFFFFF);
    }

    @Override
    public boolean equals(Object o) { // count keys as equal if they have the same key values.
      return o instanceof GInfoPC && Arrays.equals(_gs, ((GInfoPC) o)._gs); // && _val==((GInfoPC) o)._val;
    }

    @Override
    public int hashCode() {
      return _hash;
    }

    @Override
    public String toString() {
      return Arrays.toString(_gs);
    }
  }
}
