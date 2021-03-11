import io.threadcso._

/∗∗ The trait for a Sleeping Tutor protocol. ∗/
trait SleepingTutor{

    /∗∗ A tutor waits for students to arrive. ∗/
    def tutorWait

    /∗∗ A student arrives and waits for the tutorial. ∗/
    def arrive

    /∗∗ A student receives a tutorial. ∗/
    def receiveTute

    /∗∗ A tutor ends the tutorial. ∗/
    def endTeach

}

class SleepingTutorMonitor extends SleepingTutor {

    // monitor to control concurrent operations
    private val monitor = new Monitor

    // conditions to govern when threads should wake up
    private val studentsReady, tutorialStarted, tutorialEnded = monitor.newCondition

    // conditions to check that the threads should/should not be awake
    private var studentArrivedCond, studentsReadyCond, tutorialStartedCond, tutorialEndedCond = false

    def tutorWait = monitor.withLock {
        studentsReady.await(studentsReadyCond);
        tutorialStartedCond = true
        tutorialEndedCond = false
        tutorialStarted.signalAll
    }

    def arrive = monitor.withLock {
        if (!studentsArrivedCond) {
            studentsArrivedCond = true
            studentsReady.await(studentsReadyCond)
        }
        else {
            studentsReadyCond = true
            studentsReadyCond.signalAll()
        }
    }

    def receiveTute = monitor.withLock {
        tutorialStarted.await(tutorialStartedCond)
        tutorialEnded.await(tutorialEndedCond)
    }

    def endTeach = monitor.withLock {
        tutorialEndedCond = true
        tutorialEnded.signalAll()
        studentArrivedCond, studentsReadyCond, tutorialStartedCond = false
    }

}

object SleepingTutorSimulation{

    // Some implementation of SleepingTutor
    private val st: SleepingTutor = new SleepingTutorMonitor

    private def student(me: String) = proc(”Student”+me){
        while(true) {
            Thread.sleep(Random.nextInt(2000))
            println(”Student ”+me+” arrives”); st.arrive
            println(”Student ”+me+” ready for tutorial”); st.receiveTute
            println(”Student ”+me+” leaves”)
        }
    }

    private def tutor = proc(”Tutor”){
        while(true) {
            println(”Tutor waiting for students”); st.tutorWait
            println(”Tutor starts to teach”); Thread.sleep(1000)
            println(”Tutor ends tutorial”); st.endTeach
            Thread.sleep(1000)
        }
    }

    private def system = tutor || student(”Alice”) || student(”Bob”)

    def main(args: Array[String]) = run(system)
    
}