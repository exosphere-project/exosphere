module Tests.Helpers.Queue exposing (queueSuite)

import Expect
import Helpers.Queue
import Test exposing (Test, describe, test)


queueSuite : Test
queueSuite =
    describe "Queue Tests"
        [ let
            busyNo =
                Helpers.Queue.isQueueBusy
                    [ { status = Helpers.Queue.Waiting, job = "1" }
                    , { status = Helpers.Queue.Waiting, job = "2" }
                    , { status = Helpers.Queue.Waiting, job = "3" }
                    ]

            busyYes =
                Helpers.Queue.isQueueBusy
                    [ { status = Helpers.Queue.Processing, job = "1" }
                    , { status = Helpers.Queue.Waiting, job = "2" }
                    , { status = Helpers.Queue.Waiting, job = "3" }
                    ]
          in
          describe "isQueueBusy" <|
            [ test "With all jobs waiting" <|
                \_ -> Expect.equal busyNo False
            , test "With some jobs processing" <|
                \_ -> Expect.equal busyYes True
            ]
        , let
            queue =
                [ { status = Helpers.Queue.Waiting, job = "1" }
                , { status = Helpers.Queue.Waiting, job = "2" }
                , { status = Helpers.Queue.Waiting, job = "3" }
                ]

            expectedNextJobs =
                [ { status = Helpers.Queue.Processing, job = "1" }
                ]

            expectedNextQueue =
                [ { status = Helpers.Queue.Processing, job = "1" }
                , { status = Helpers.Queue.Waiting, job = "2" }
                , { status = Helpers.Queue.Waiting, job = "3" }
                ]

            ( nextJobs, nextQueue ) =
                Helpers.Queue.marshalQueue queue
          in
          describe "marshalQueue" <|
            [ test "Returns next jobs" <|
                \_ ->
                    Expect.equal nextJobs expectedNextJobs
            , test "Returns an updated queue" <|
                \_ ->
                    Expect.equal nextQueue expectedNextQueue
            ]
        , let
            queue =
                [ { status = Helpers.Queue.Processing, job = "1" }
                , { status = Helpers.Queue.Waiting, job = "2" }
                ]

            newJobs =
                [ "3", "4" ]

            expectedQueue =
                [ { status = Helpers.Queue.Processing, job = "1" }
                , { status = Helpers.Queue.Waiting, job = "2" }
                , { status = Helpers.Queue.Waiting, job = "3" }
                , { status = Helpers.Queue.Waiting, job = "4" }
                ]

            updatedQueue =
                Helpers.Queue.addJobsToQueue newJobs queue
          in
          describe "addJobsToQueue" <|
            [ test "Adds the specified jobs to the queue" <|
                \_ ->
                    Expect.equal updatedQueue expectedQueue
            , test "Appends them without changing the order" <|
                \_ ->
                    Expect.equal
                        (List.map .job updatedQueue)
                        [ "1", "2", "3", "4" ]
            ]
        , let
            queue =
                [ { status = Helpers.Queue.Processing, job = "1" }
                , { status = Helpers.Queue.Waiting, job = "2" }
                , { status = Helpers.Queue.Waiting, job = "3" }
                ]

            finishedJob =
                "1"

            expectedQueue =
                [ { status = Helpers.Queue.Waiting, job = "2" }
                , { status = Helpers.Queue.Waiting, job = "3" }
                ]

            updatedQueue =
                Helpers.Queue.removeJobFromQueue finishedJob queue
          in
          describe "removeJobFromQueue" <|
            [ test "Removes the specified job from the queue" <|
                \_ ->
                    Expect.equal updatedQueue expectedQueue
            , test "Does not modify other jobs in the queue" <|
                \_ ->
                    Expect.equal
                        (List.map .job updatedQueue)
                        [ "2", "3" ]
            ]
        ]
