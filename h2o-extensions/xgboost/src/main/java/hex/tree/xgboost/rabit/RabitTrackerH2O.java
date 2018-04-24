package hex.tree.xgboost.rabit;

import hex.tree.xgboost.rabit.util.LinkMap;
import ml.dmlc.xgboost4j.java.IRabitTracker;
import water.*;
import water.util.Log;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.Socket;
import java.nio.channels.ServerSocketChannel;
import java.nio.channels.SocketChannel;
import java.util.*;

public class RabitTrackerH2O implements IRabitTracker {
    public static final int MAGIC = 0xff99;
    private ServerSocketChannel sock;
    private int port = 9091;

    private int workers;

    private Map<String, String> envs = new HashMap<>();

    private RabitTrackerH2OThread trackerThread;

    public RabitTrackerH2O(int workers) {
        super();

        if(workers < 1) {
            throw new IllegalStateException("workers must be greater than or equal to one (1).");
        }

        this.workers = workers;
        Log.debug("Rabit tracker started on port ", this.port);
    }

    @Override
    public Map<String, String> getWorkerEnvs() {
        envs.put("DMLC_NUM_WORKER", String.valueOf(workers));
        envs.put("DMLC_NUM_SERVER", "0");
        envs.put("DMLC_TRACKER_URI", H2O.SELF_ADDRESS.getHostAddress());
        envs.put("DMLC_TRACKER_PORT", Integer.toString(port));
        envs.put("rabit_world_size", Integer.toString(workers));

        return envs;
    }

    @Override
    public boolean start(long timeout) {
        boolean tryToBind = true;
        while(tryToBind) {
            try {
                this.sock = ServerSocketChannel.open();
                this.sock.socket().setReceiveBufferSize(64 * 1024);
                InetSocketAddress isa = new InetSocketAddress(H2O.SELF_ADDRESS, this.port);
                this.sock.socket().bind(isa);
                tryToBind = false;
            } catch (java.io.IOException e) {
                this.port++;
                if(this.port > 9999) {
                    throw new RuntimeException("Failed to bind Rabit tracker to a socket in range 9091-9999", e);
                }
            }
        }

        if(null != this.trackerThread) {
            throw new IllegalStateException("Rabit tracker already started.");
        }
        RabitTrackerH2OThread trackerThread = new RabitTrackerH2OThread(this);
        trackerThread.setDaemon(true);
        trackerThread.start();
        this.trackerThread = trackerThread;
        return true;
    }

    @Override
    public void stop() {
        if(null != this.trackerThread) {
            this.trackerThread.interrupt();
            this.trackerThread = null;

            try {
                this.sock.close();
            } catch (IOException e) {
                throw new RuntimeException("Failed to close Rabit tracker socket.f", e);
            } finally {
                this.port = 9091;
            }
        }
    }

    private class RabitTrackerH2OThread extends Thread {
        private RabitTrackerH2O tracker;

        private LinkMap linkMap;
        private Map<String, Integer> jobToRankMap = new HashMap<>();

        private RabitTrackerH2OThread(RabitTrackerH2O tracker) {
            setPriority(MAX_PRIORITY-1);
            this.setName("TCP-" + tracker.sock);
            this.tracker = tracker;
        }


        private static final String PRINT_CMD = "print";
        private static final String SHUTDOWN_CMD = "shutdown";
        private static final String START_CMD = "start";
        private static final String RECOVER_CMD = "recover";
        private static final String NULL_STR = "null";

        @Override
        public void run() {
            Set<Integer> shutdown = new HashSet<>();
            Map<Integer, RabitWorker> waitConn = new HashMap<>();
            List<RabitWorker> pending = new ArrayList<>();
            Queue<Integer> todoNodes = new ArrayDeque<>(tracker.workers);
            while (!interrupted() && shutdown.size() != tracker.workers) {
                try{
                    final SocketChannel channel = tracker.sock.accept();
                    final RabitWorker worker = new RabitWorker(channel);

                    if (PRINT_CMD.equals(worker.cmd)) {
                        String msg = worker.receiver().getStr();
                        Log.warn("Rabit worker: ", msg);
                        continue;
                    } else if (SHUTDOWN_CMD.equals(worker.cmd)) {
                        assert worker.rank >= 0 && !shutdown.contains(worker.rank);
                        assert !waitConn.containsKey(worker);
                        shutdown.add(worker.rank);
                        channel.close();
                        Log.debug("Received ", worker.cmd, " signal from ", worker.rank);
                        continue;
                    }
                    assert START_CMD.equals(worker.cmd) || RECOVER_CMD.equals(worker.cmd);

                    if (null == linkMap) {
                        assert START_CMD.equals(worker.cmd);
                        linkMap = new LinkMap(tracker.workers);
                        for (int i = 0; i < tracker.workers; i++) {
                            todoNodes.add(i);
                        }
                    } else {
                        assert worker.worldSize == -1 || worker.worldSize == tracker.workers;
                    }

                    if (RECOVER_CMD.equals(worker.cmd)) {
                        assert worker.rank >= 0;
                    }

                    int rank = worker.decideRank(jobToRankMap);
                    if (-1 == rank) {
                        assert todoNodes.size() != 0;
                        pending.add(worker);
                        if (pending.size() == todoNodes.size()) {
                            Collections.sort(pending);
                            for (RabitWorker p : pending) {
                                rank = todoNodes.poll();
                                if (!NULL_STR.equals(p.jobId)) {
                                    jobToRankMap.put(p.jobId, rank);
                                }
                                p.assignRank(rank, waitConn, linkMap);

                                if (p.waitAccept > 0) {
                                    waitConn.put(rank, p);
                                }

                                Log.debug("Received " + p.cmd +
                                        " signal from " + p.host + ":" + p.workerPort +
                                        ". Assigned rank " + p.rank
                                );
                            }
                        }
                        if (todoNodes.isEmpty()) {
                            Log.debug("All " + tracker.workers + " Rabit workers are getting started.");
                        }
                    } else {
                        worker.assignRank(rank, waitConn, linkMap);
                        if (worker.waitAccept > 0) {
                            waitConn.put(rank, worker);
                        }
                    }
                } catch (IOException e) {
                    Log.info("Exception in Rabit tracker.", e);
                    Log.err(e);
                }
            }
            Log.debug("All Rabit nodes finished.");
        }
    }

    @Override
    public int waitFor(long timeout) {
        while(null != this.trackerThread && this.trackerThread.isAlive()) {
            try {
                this.trackerThread.join(timeout);
                this.trackerThread = null;
            } catch (InterruptedException e) {
                Log.debug("Rabit tracker thread got suddenly interrupted.", e);
            }
        }
        return 0;
    }

    @Override
    public void uncaughtException(Thread t, Throwable e) {
        stop();
    }
}
