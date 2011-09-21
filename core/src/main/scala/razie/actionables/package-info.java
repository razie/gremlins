/**
 * Abstraction of the concept of action and associated support.
 *
 * <p>
 * A successful platform needs to simplify the definition of actions, override and injection of actions. 
 * Users should be able to easily customize the objects inside the application, their commands (menus whatnot). 
 * 
 * <p>For instance, I want to "play" a movie by first logging who/what/when and then default to what "play" used to do. 
 * Or I want to forbid playing altogether. You can a) complicate things with configurable notification 
 * frameworks, pre/post actions, authorization frameworks etc or b) let me override the default behavior 
 * of your "actionables".
 * 
 * <p>ActionItem is the basic action concept. We're extending it as a function concept (Executable), 
 * which accepts input and output.
 * 
 * <p>Executables are things that can do something (functions). Actionables are prepared invocations.
 * 
 * <p>Actionables can be invoked to do something: method calls, scripts, functions, commands, URLs etc...
 * 
 * <p>I had an older Actionable framework which I'm now merging with the gremlins workflow framework...yeah, it's not pretty right now
 * 
 * TODO complete moving allthe stuff in here - including how to override existing actions, with examples etc. 
 * Right now the media players can override their actions via configuration... that's how the VNC player is invoked (telnet)
 * 
 * @version $Id: package-info.java,v 1.1 2007-10-02 11:54:36 razvanc Exp $
 */
package razie.actionables;

