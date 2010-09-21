/**
 * life-time management
 * 
 * use of Thread is strictly forbidden, since it doesn't offer enough functionality. Instead, use
 * the Worker and Being with the Lifegiver.
 * 
 * All active objects must always be able to say what they're doing and be prepared to die. The idea
 * is that the user can always tell what the heck is going on and can kill anyone he/she doesn't
 * like. Don't we hate it when the hard-drive keeps the computer frozen and we don't know what the
 * hell is going on?
 * 
 * There are two types of active objects:
 * 
 * Workers are completing a specific task and they must set a progress indicator. Once their task is
 * complete, they finish and die. For instance background compilation, a file search etc. They
 * borrow life.
 * 
 * Breathers are alive all the time and keep breathing on a schedule - that's when they do
 * something. They must not take long to do whatever and must say what they're doing at all times.
 * 
 * For computer dorks: Worker is a Worker, Breather is a scheduled task (cron job). I don't like
 * sleeping beauties, so there will be an event thing, although for now you can use a Worker.
 * 
 * TODO roadmap: read/write lock, prod/cons and pipes, parallel processing
 * 
 * @version $Id: package-info.java,v 1.1 2007-10-02 11:54:36 razvanc Exp $
 */
package com.razie.pubstage.life;

