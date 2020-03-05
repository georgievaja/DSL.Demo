namespace DSL.Teams

open Team.FunctionTypes
open DSL.Types
open Team.Types
open Team.PublicTypes

module TeamFunctions =
    open Team.Monads
    open Team.Logs

    let validateTeams : ValidateTeams =

        fun unvalidatedTeams ->
            // dummy, add validation

            let mapParameters (parameters: UnvalidatedParameter list) =
               parameters
                |> List.map (fun p -> (ParameterName.create p.Name, ParameterValue.create p.Value))
                |> Map.ofList

            let teams =
                unvalidatedTeams
                |> UnvalidatedTeams.value
                |> List.map (fun t -> (TeamName.create t.Name, mapParameters t.Parameters))
                |> Map.ofList
                |> Teams.create


            Success (teams, [])

    module TeamExistsFunctions = 
        let checkTeamExists : CheckTeamExists =
                fun key teams ->
                            teams
                            |> Teams.value
                            |> Map.tryFind key
                            |> Option.toBool
                            |> fun result -> Success (result, [(FunctionUsedKey key)])


        let checkTeamExistsWorkflow : CheckTeamExistsWorkflow =    
            fun onValidateTeams onCheckTeamNameExistence key ->

                onValidateTeams 
                >=> onCheckTeamNameExistence key


        let workflow key =                          
            let teamKey = TeamName.create key
            checkTeamExistsWorkflow validateTeams checkTeamExists teamKey


            
