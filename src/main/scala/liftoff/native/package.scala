package liftoff

import liftoff.coroutine._

import java.lang.foreign._

package object native {
  

  object Native {

    def createArena(): Arena = Coroutine.currentScope match {
      // We are running single threaded (per model) so no shared memory needed
      case Some(s: ContinuationCoroutineScope) => Arena.ofConfined()
      case _ => Arena.ofShared()
    }



  }

}
