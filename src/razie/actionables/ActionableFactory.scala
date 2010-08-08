package razie.actionables;

import razie.base.ActionItem;
import razie.base.AttrAccess;
import razie.base.AttrAccessImpl;

/**
 * simple actionable factory
 * 
 * TODO 1-1 detailed docs
 * 
 * @author razvanc
 */
object ActionableFactory {
   private var singleton:Option[ActionableFactory] = None
   
   def init(f:ActionableFactory ) { singleton = Some(f) }

   def instance : ActionableFactory = singleton getOrElse {throw new IllegalStateException ("NEEDS initialized - I think ActFactory does it...") }

   def make(act:Actionable , newex:IExecutable ) : Actionable = 
      instance.make(act.name, act.item, act.inArgs, newex)

   /** Foo = (Foo) makeProxy (foo, overloadingActionables) */
   def makeProxy(o:Object , hasa:HasActionables) : AnyRef =  DebugProxy.newInstance(o, hasa);

}

trait ActionableFactory {

   def make(name:String , ai:ActionItem , in:AttrAccess , ex:IExecutable ) : Actionable

   /** standard make from factory... */
   def make(name:String , unifiedstring:String ) : Actionable

}
