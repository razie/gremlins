package com.razie.pub.actionables;

import razie.base.AttrAccess;

/** basic interface for an executable / task / activity
 * 
 * @author razvanc
 */
public interface IExecutable {
   public AttrAccess execute(AttrAccess in);
}
