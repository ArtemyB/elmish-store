module ElmishStore.Example.ModelStore

open Elmish
open ElmishStore
open ElmishStore.Example.Model
open Feliz

#if DEBUG

open Elmish.Debug

#endif

let storesHost = ElmishStoresHost<string>()

let store =
    let program =
        Program.mkProgram init update (fun _ _ -> ())
        #if DEBUG
        |> Program.withConsoleTrace
        |> Program.withDebugger
        #endif
    storesHost.create "main" program

[<Hook>]
let useSelector (selector: Model -> 'a) = React.useElmishStore (store, selector)

[<Hook>]
let useSelectorMemoized (memoizedSelector: Model -> 'a) =
  React.useElmishStoreMemoized (store, memoizedSelector)

let dispatch = store.Dispatch