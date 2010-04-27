package razie.actionables;

import java.util.Map;

/** represents a collection of actionables, i.e. the methods of an object
 *
 * @author razvanc
 */
public interface HasActionables {
   public Map<String, Actionable> getActionables();
}
