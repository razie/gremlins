/**  ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package com.razie.pubstage.life;

import java.util.HashMap;
import java.util.Map;

import razie.base.ActionItem;
import razie.base.life.Being;

import com.razie.pub.base.ExecutionContext;
import com.razie.pub.base.log.Log;
//import com.razie.pub.events.PostOffice;

/**
 * You should always check to see if you're dying...shhh, you'll be killed soon if you don't...
 * 
 * <code>
 * 
 * @Override public void process() { while (!this.dying) { ... // sleep if you must but you should
 *           be a breather instead... try { sleep(5 * 60 * 1000); } catch (InterruptedException e) {
 *           } } } </code>
 * 
 * @author razvanc99
 */
public abstract class Worker implements Runnable, Being {

    private ActionItem    progressCode;
    private ActionItem    me;
    protected ExecutionContext threadCtx;

    private ActionItem    SLEEPING = new ActionItem("sleeping...");

    public Worker(ActionItem me, ExecutionContext threadCtx) {
        this.me = me;
        this.threadCtx = threadCtx;
    }

    public Worker(ActionItem me) {
        this.me = me;
    }

    /**
     * return a 0-100 progress indicator. Be sure to synchronize inside on whatever you deem
     * neccessary
     */
    public int getProgress() {
        return progress;
    }

    /**
     * this would also update the progress message, if any
     * 
     * @param p a progress indicator 0-nothing done, 100-all done
     * @param progressCode a string code for progress. Derived classes must overload the
     *        getProgressMsg to translate the code
     */
    public void setProgress(int p, ActionItem progressCd) {
        this.progress = p;
        this.progressCode = progressCd;
        this.notifyUpdated();
    }

    /**
     * this would also update the progress message, if any
     * 
     * @param p a progress indicator 0-nothing done, 100-all done
     * @param progressCode a string code for progress. Derived classes must overload the
     *        getProgressMsg to translate the code
     */
    public void setProgress(int p, String progressCd) {
        this.progress = p;
        // this.progressCode = progressCd;
        this.notifyUpdated();
    }

    /**
     * Notification that the thread is starting (called before 'process' function)
     * 
     * <p>
     * Don't forget to also check for the shutdownRequested() and if set, throw InterruptedException
     * 
     * @param p a progress indicator 0-nothing done, 100-all done
     * @param progressCode a string code for progress. Derived classes must overload the
     *        getProgressMsg to translate the code
     */
    public static void updateProgress(int newProgress, ActionItem newProgressCode) {
        Worker w = findMe();
        if (w != null) {
            synchronized (w) {
                w.setProgress(newProgress, newProgressCode);
            }
        }
    }

    /**
     * find the current Worker
     */
    public static Worker findMe() {
        Worker w = null;
        synchronized (allRq) {
            w = (Worker) allRq.get(Thread.currentThread().getName());
        }

        return w;
    }

    /**
     * Notification that the thread is starting (called before 'process' function)
     * 
     * <p>
     * Don't forget to also check for the shutdownRequested() and if set, throw InterruptedException
     * 
     * @param p a progress indicator 0-nothing done, 100-all done
     * @param progressCode a string code for progress. Derived classes must overload the
     *        getProgressMsg to translate the code
     */
    public static void updateProgress(int newProgress, String newProgressCode) {
        Worker w = findMe();
        if (w != null) {
            synchronized (w) {
                w.setProgress(newProgress, newProgressCode);
            }
        }
    }

    /**
     * stops the current execution (*unsafe one). Use the shutdown(), sleep 5 seconds and only if
     * the thread is not done, then use cancel()...
     */
    @SuppressWarnings("deprecation")
    public void cancel() {
        if (this.intState != IntState.STOPPED) {
            Thread.currentThread().stop();
            // Notification that the thread has stopped (called after 'process' function)
            notifyStopped();
        }
    }

    /**
     * Abstract function that is called from <b>run</b> function of the thread. This should be
     * implemented by each child class.
     */
    public abstract void process();

    /**
     * internal run - do not overwrite. Overwrite process() instead.
     */
    final public void run() {
        // Notification that the thread is starting (called before 'process' function)
        this.setIntState(IntState.STARTED);

        synchronized (allRq) {
            allRq.put(Thread.currentThread().getName(), this);
        }
        // real thread (run) function
        try {
            if (this.threadCtx != null)
                this.threadCtx.enter();
            
            process();
        } catch (Throwable t) {
            if (t instanceof ThreadDeath) {
                logger.log("Thread stopped with ThreaDeath, not nice !");
                this.setIntState(IntState.STOPPED);

                // Notification that the thread has stopped (called after 'process' function)
                notifyStopped();

                // ThreadDeath must be propagated
                throw (ThreadDeath) t;
            } else if (Worker.getRootCause(t) instanceof BeingDyingRtException) {
                logger.log("Thread stopped at user request...");
            } else {
                logger.alarm(t.getMessage(), t);
            }
        } finally {
            if (this.threadCtx != null)
                ExecutionContext.exit();
        }
        
        this.setIntState(IntState.STOPPED);

        // Notification that the thread has stopped (called after 'process' function)
        notifyStopped();
    }

    /**
     * Notification that the thread has stopped (called after 'process' function)
     */
    final private void notifyStopped() {
//        PostOffice.shout(null, LifeEventTypes.EV_THREAD_END, "subject", this);

        synchronized (allRq) {
            allRq.remove(Thread.currentThread().getName());
        }
    }

    /**
     * Notification that the thread has stopped (called after 'process' function)
     */
    final private void notifyUpdated() {
//        PostOffice.shout(null, LifeEventTypes.EV_THREAD_UPD, "subject", this);
    }

    /**
     * graceful thread stop - the framework will set this 5 seconds before thread is killed. Client
     * threads should check this every now and then and if true, NOT exit or return, but throw
     * GracefulShutdownRtException. This specific exception will not be reported...
     * 
     * <p>
     * Implementing work items should check this flag before doing anything of duration and just
     * stop if the flag is true.
     * 
     * @return true if the thread should stop...
     */
    public static boolean dying() {
        Worker w = findMe();
        if (w != null) {
            synchronized (w) {
                return w.dying;
            }
        }

        // it's a thread not controlled by our framework - just keep doing whatever...
        return false;
    }

    /**
     * graceful thread stop - the framework will set this 5 seconds before thread is killed. Client
     * threads should check this every now and then and if true, NOT exit or return, but throw
     * BeingDyingRtException. This specific exception will not be reported...
     * 
     * @return true if the thread should stop...
     */
    public void shutdown() {
        synchronized (this) {
            this.dying = true;
        }
    }

    void setIntState(IntState intState) {
        this.intState = intState;
    }

    /** check if work is done */
    public boolean isDone() {
        return this.intState == IntState.STOPPED;
    }

    /**
     * the count of the thread
     */
    public long       _index   = 0;

    protected int     progress = 0;

    /** this is set to true 5 sec before thread is killed. Client code should check this and stop. */
    protected boolean dying    = false;

    /** started=1, stoped=0 */
    static enum IntState {
        STOPPED, STARTED
    }

    private IntState   intState = IntState.STOPPED;
    private ActionItem progressWhileSleeping;

    /** beings should be nice and answer what they're doing right now (status in nerdsspeak) */
    public ActionItem whatAreYouDoing() {
        return this.progressCode;
    }

    /** beings should be nice and tell who they are */
    public ActionItem whoAreYou() {
        return this.me;
    }

    /** use instead of Thread.sleep() */
    public static void sleep(long millis) throws InterruptedException {
        Worker w = findMe();
        if (w != null) {
            w.gotosleep(millis);
        } else {
            // arsch
            Thread.sleep(millis);
        }
    }

    /** not sure why you'd overload this, but hey...go thrill yourself */
    protected void gotosleep(long millis) throws InterruptedException {
        this.progressWhileSleeping = this.progressCode;
        this.progressCode = SLEEPING;

        try {
            Thread.sleep(millis);
        } catch (InterruptedException e) {
            throw e;
        } finally {
            this.progressCode = this.progressWhileSleeping;
            this.progressWhileSleeping = null;
        }
    }

    /**
     * simply recurse to get the root cause
     */
    public static Throwable getRootCause(Throwable aThrowable) {
        Throwable root = aThrowable;
    
        while (root != null && root.getCause() != null) {
            root = root.getCause();
        }
    
        return root;
    }

    /*
     * Thread listeners list; listener that get the notifications when the thread starts/stops
     */
    /** Map<String taskname, MTWrkRq> all wrk rq in progress */
    private static Map<String, Worker> allRq  = new HashMap<String, Worker>();
    private static final Log           logger = Log.factory.create("", Worker.class.getName());
}
