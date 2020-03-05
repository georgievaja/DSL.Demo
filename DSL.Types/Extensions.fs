namespace DSL.Types

module Option =
    let toBool op =
        match op with
            | Some _ -> true
            | None -> false
