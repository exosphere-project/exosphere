module Helpers.ServerActionRequestQueue exposing (marshalServerActionRequestQueue)

import Dict
import Helpers.GetterSetters as GetterSetters
import Helpers.Queue
import OpenStack.Types as OSTypes exposing (ServerUuid)
import Rest.Nova
import Types.Project exposing (Project)
import Types.ServerActionRequestQueue as ServerActionRequestQueue
import Types.SharedMsg exposing (SharedMsg)


marshalServerActionRequestQueue :
    Project
    -> ServerUuid
    -> List OSTypes.ServerSecurityGroupUpdate
    -> ( Project, Cmd SharedMsg )
marshalServerActionRequestQueue project serverId serverSecurityGroupUpdates =
    let
        queue =
            GetterSetters.getServerActionRequestQueue project serverId

        -- Add new removal requests to a queue.
        removalRequests =
            serverSecurityGroupUpdates
                |> List.filterMap
                    (\sgUpdate ->
                        case sgUpdate of
                            OSTypes.RemoveServerSecurityGroup serverSecurityGroup ->
                                Just <| ServerActionRequestQueue.RemoveServerSecurityGroup serverSecurityGroup

                            _ ->
                                Nothing
                    )

        updatedQueue =
            Helpers.Queue.addJobsToQueue removalRequests queue

        ( nextJobs, newQueue ) =
            Helpers.Queue.marshalQueue updatedQueue

        newServerActionRequestQueue =
            Dict.insert
                serverId
                newQueue
                project.serverActionRequestQueue

        newProject =
            { project
                | serverActionRequestQueue = newServerActionRequestQueue
            }

        -- If we have jobs to process, we can execute them.
        removeRequests =
            nextJobs
                |> List.map
                    (\job ->
                        case job.job of
                            ServerActionRequestQueue.RemoveServerSecurityGroup serverSecurityGroup ->
                                Rest.Nova.requestUpdateServerSecurityGroup newProject serverId <|
                                    OSTypes.RemoveServerSecurityGroup
                                        { uuid = serverSecurityGroup.uuid
                                        , name = serverSecurityGroup.name
                                        }
                    )

        -- Requests to add server security groups can be concurrent.
        addRequests =
            serverSecurityGroupUpdates
                |> List.filterMap
                    (\sgUpdate ->
                        case sgUpdate of
                            OSTypes.AddServerSecurityGroup _ ->
                                Just <| Rest.Nova.requestUpdateServerSecurityGroup newProject serverId <| sgUpdate

                            OSTypes.RemoveServerSecurityGroup _ ->
                                Nothing
                    )
    in
    ( newProject, Cmd.batch (removeRequests ++ addRequests) )
