/**  ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package com.razie.pubstage.life;


/**
 * long-running threads should check the MTWrkRq.shutdownRequested() and throw this exception - it
 * should just flow right back to here...
 * 
 * @author razvanc99
 * 
 * @stereotype exception
 */
@SuppressWarnings("serial")
public class BeingDyingRtException extends RuntimeException {
    /**
     * Constructs a new exception with null as its detail message. The cause is not initialized, and
     * may subsequently be initialized by a call to Throwable.initCause(java.lang.Throwable).
     */
    public BeingDyingRtException() {
        super("Graceful thread stop requested...");
    }

}
