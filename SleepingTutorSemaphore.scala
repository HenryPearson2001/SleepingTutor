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

class SleepingTutorSemaphore extends SleepingTutor {

    // mutex to control concurrent operations
    private val mutex = MutexSemaphore()

    // signalling semaphores to govern which threads should be active
    private val studentArrived, studentReady, studentsReady, tutorialStarted, tutorialEnded = SignallingSemaphore()

    // ints to track how many students are ready
    private var studentsArrivedCount, studentsReadyCount, studentsInTuteCount = 0

    def tutorWait = {
        studentsReady.down
        tutorialStarted.up
    }

    def arrive = {
        mutex.down
        if (studentsArrivedCount < 1) {
            studentsArrivedCount += 1
            mutex.up
            studentArrived.down
            studentsArrivedCount -= 1
            if (studentsArrivedCount > 0) {
                studentArrived.up
            }
            else {
                mutex.up
            }
        }
        else {
            studentArrived.up
        }
    }

    def receiveTute = {
        mutex.down
        studentsInTuteCount += 1
        if (studentsReadyCount < 1) {
            studentsReadyCount += 1
            mutex.up
            studentReady.down
            studentsReadyCount -= 1
            if (studentsReadyCount > 0) {
                studentReady.up
            }
            else {
                studentsReady.up
            }
        }
        else {
            studentReady.up
        }
        tutorialEnded.down
        studentsInTuteCount -= 1
        if (studentsInTuteCount > 0) {
            tutorialEnded.up
        }
        else {
            mutex.up
        }
    }

    def endTeach = {
        tutorialStarted.down
        tutorialEnded.up
    }

}

import scala.util.Random

object SleepingTutorSemaphoreSimulation{

    // Some implementation of SleepingTutor
    private val st: SleepingTutor = new SleepingTutorSemaphore

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