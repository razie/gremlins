

import junit.framework.Test;
import junit.framework.TestSuite;

import org.scalatest.junit._
import org.scalatest.SuperSuite

/** main test suite */
class SuiteWfNew extends SuperSuite (
  List (
    new razie.wf.WfLibTest
  )
)

