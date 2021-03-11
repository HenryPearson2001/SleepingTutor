import io.threadcso._

// The trait for a Sleeping Tutor protocol.
trait SleepingTutor{

    // A tutor waits for students to arrive.
    def tutorWait

    // A student arrives and waits for the tutorial.
    def arrive

    // A student receives a tutorial.
    def receiveTute

    // A tutor ends the tutorial.
    def endTeach

}

class SleepingTutorMonitor extends SleepingTutor {

    // monitor to control concurrent operations
    private val monitor = new Monitor

    // conditions to govern when threads should wake up
    private val studentsArrived, studentsReady, tutorialStarted, tutorialEnded = monitor.newCondition

    // conditions to check that the threads should/should not be awake
    private var studentArrivedCond, studentsArrivedCond, studentReadyCond, studentsReadyCond, tutorialStartedCond, tutorialEndedCond = false

    def tutorWait = monitor.withLock {
        studentsReady.await(studentsReadyCond);
        tutorialStartedCond = true
        tutorialEndedCond = false
        tutorialStarted.signalAll
    }

    def arrive = monitor.withLock {
        if (!studentArrivedCond) {
            studentArrivedCond = true
            studentsArrived.await(studentsArrivedCond)
        }
        else {
            studentsArrivedCond = true
            studentsArrived.signalAll()
        }
    }

    def receiveTute = monitor.withLock {
        if (!studentReadyCond) {
            studentReadyCond = true
            studentsReady.await(studentsReadyCond)
        }
        else {
            studentsReadyCond = true
            studentsReady.signalAll
        }
        tutorialStarted.await(tutorialStartedCond)
        tutorialEnded.await(tutorialEndedCond)
    }

    def endTeach = monitor.withLock {
        tutorialEndedCond = true
        tutorialEnded.signalAll()
        studentArrivedCond = false
        studentsArrivedCond = false
        studentReadyCond = false
        studentsReadyCond = false
        tutorialStartedCond = false
    }

}

import scala.util.Random

object SleepingTutorMonitorSimulation{

    // Some implementation of SleepingTutor
    private val st: SleepingTutor = new SleepingTutorMonitor

    private def student(me: String) = proc{
        while(true) {
            Thread.sleep(Random.nextInt(2000))
            println("Student "+me+" arrives"); st.arrive
            println("Student "+me+" ready for tutorial"); st.receiveTute
            println("Student "+me+" leaves")
        }
    }

    private def tutor = proc("Tutor"){
        while(true) {
            println("Tutor waiting for students"); st.tutorWait
            println("Tutor starts to teach"); Thread.sleep(1000)
            println("Tutor ends tutorial"); st.endTeach
            Thread.sleep(1000)
        }
    }

    private def system = tutor || student("Alice") || student("Bob")

    def main(args: Array[String]) = run(system)

}