module Helpers.Queue exposing (Job, JobStatus(..), isQueueBusy, marshalQueue, removeJobFromQueue)

import List.Extra


type JobStatus
    = -- Waiting means this job is in the queue to be processed.
      Waiting
      -- Processing means we are working on a job that has not yet finished.
    | Processing


type alias Job a =
    { a
        | status : JobStatus
    }


concurrency : number
concurrency =
    1


isQueueBusy : List (Job a) -> Bool
isQueueBusy queue =
    queue
        |> List.filter (\job -> job.status == Processing)
        |> List.length
        |> (\count -> count >= concurrency)


removeJobFromQueue : Job a -> List (Job a) -> List (Job a)
removeJobFromQueue jobToRemove queue =
    queue
        |> List.filter (\job -> job /= jobToRemove)


{-| From the current queue, return

  - the next jobs to process and
  - an updated instance of the queue.

-}
marshalQueue : List (Job a) -> ( List (Job a), List (Job a) )
marshalQueue queue =
    let
        numberPending =
            queue
                |> List.filter (\job -> job.status == Processing)
                |> List.length

        placesToFill =
            concurrency - numberPending

        nextJobIndices =
            queue
                |> List.Extra.findIndices (\job -> job.status == Waiting)
                |> List.take placesToFill
    in
    queue
        |> List.Extra.indexedFoldl
            (\index job ( j, q ) ->
                if List.member index nextJobIndices then
                    let
                        pendingJob =
                            { job | status = Processing }
                    in
                    ( j ++ [ pendingJob ], q ++ [ pendingJob ] )

                else
                    ( j, q ++ [ job ] )
            )
            ( [], [] )
