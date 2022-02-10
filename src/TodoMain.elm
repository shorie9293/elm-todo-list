port module TodoMain exposing ( main
                          , initTodoModel
                          , reduceEnemyHp
                          , encountNextEnemy
                          , judgeLevelUp
                          , setLoginInformation)
import Routes
import Url exposing (Url)

import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Navigation
import Html exposing ( .. )
import Html.Lazy exposing (lazy)
import Html.Attributes exposing ( .. )
import Html.Events exposing ( .. )
import Html.Events.Extra exposing ( onChange )
import Json.Decode as Decode
import Json.Decode.Pipeline as DP
import Json.Encode as Encode
import Time as T
import Time.Extra as TE
import Task as Ts
import UUID exposing (UUID)
import Random
import String exposing (fromInt)

-- MAIN
main : Program Encode.Value Model Msg
main = Browser.application
  { init = init
  , view = view
  , update = updateWithStorage
  , subscriptions = subscriptions
  , onUrlRequest = Visit
  , onUrlChange = Routes.match >> NewRoute
  }


-- MODEL

type Page
  = Todo
  | Buttle
  | NotFound

type alias Task =
  { id : String
  , date : Int
  , checked : Bool
  , task : String
  , project : String
  , taskType : String
  , repeatTask : String
  , repeatedDay : List String
  , repeatedDate : List String
  }

projectTypes : List (Int, String)
projectTypes =
  [ (1, "メイン")
  , (2, "サブ")
  , (3, "繰り返し")
  ]

tasktypes : List (Int, String)
tasktypes =
  [ (1, "次の行動")
  , (2, "連絡待ち")
  , (3, "待機")
  ]

type alias EnemyModel =
  { id : Int
  , enemyHp : Int
  , lastEnemyHp : Int
  }

type alias ActorModel =
  { id : Int
  , exp : Int
  , level : Int
  , levelFlag : Bool
  , point : Int
  , attack : Int}

type alias ButtleModel =
  { enemy : EnemyModel
  , actor : ActorModel
  }

type alias IndexModel =
  { status : Maybe ButtleModel
  , todos : Maybe (List Task)
  , loginStatus : Maybe LoginStatus
  }

type alias LoginStatus =
  { loginToday : Bool
  , loginDate : Int
  }

type alias Model =
  { page : Page
  , navigationKey : Navigation.Key
  , buttle : ButtleModel
  , taskList : List Task
  , task : Task
  , uid : String
  , loginStatus : LoginStatus
  , inputWindowViewVisibility : Bool
  , selectedProject : String
  }


initEnemyModel : EnemyModel
initEnemyModel =
  { id = 0
  , enemyHp = 10
  , lastEnemyHp = 10
  }


initActorModel : ActorModel
initActorModel =
  { id = 0
  , exp = 0
  , level = 1
  , levelFlag = False
  , point = 0
  , attack = 1
  }

initButtleModel : ButtleModel
initButtleModel =
  { enemy = initEnemyModel
  , actor = initActorModel
  }

initTodoModel : List Task
initTodoModel =
  []


initTask : Task
initTask =
  let
    firstProject = 
      case List.head projectTypes of
        Just p -> p
        Nothing -> (0, "")   
    firstTaskType = 
      case List.head tasktypes of
        Just t -> t
        Nothing -> (0, "")   
  in
  { id =""
  , date = 0
  , checked = False
  , task = ""
  , project = (Tuple.second firstProject)
  , taskType = (Tuple.second firstTaskType)
  , repeatTask = "-"
  , repeatedDay = [""]
  , repeatedDate = [""]
  }

initLoginStatus : LoginStatus
initLoginStatus =
  { loginToday = False
  , loginDate = 0
  }

initModel : Navigation.Key -> Model
initModel navigationKey =
  let
    initProject =
      case List.head projectTypes of
        Just p -> p
        Nothing -> (0, "Not Found") 
  in
  { page = NotFound
  , navigationKey = navigationKey
  , buttle = initButtleModel
  , taskList = initTodoModel
  , task = initTask
  , uid = ""
  , inputWindowViewVisibility = False
  , selectedProject = Tuple.second initProject
  , loginStatus = initLoginStatus
  }


init : Encode.Value -> Url -> Navigation.Key -> (Model, Cmd Msg)
init flags url navigationKey =
  case Decode.decodeValue indexDecoder flags of
    Ok model -> setNewModel model navigationKey 
                |> setNewPage (Routes.match url)
    Err _ -> setNewPage (Routes.match url) (initModel navigationKey, Cmd.none)

  
-- UPDATE
type Msg 
  = AttackToEnemy
  | NewRoute (Maybe Routes.Route)
  | Visit UrlRequest
  | AddToTask Task
  | DeleteTask Task
  | UpdateTask Task
  | Tick (T.Posix, T.Zone)
  | NewId UUID
  | ChangeChecked Task
  | ShowInputWindow Bool
  | SelectProjectTab String
  | SelectRepeatType TaskRepeatType Bool
  | LoginInformation (T.Posix, T.Zone)
  | CloseLoginWindow

type InputType
  = Project
  | TaskType
  | TaskName
  | RepeatTask

type TaskRepeatType
  = Weekly String
  | Monthly String

attackToEnemy : ButtleModel -> ButtleModel
attackToEnemy model =
  reduceEnemyHp model
  |> encountNextEnemy
  |> judgeLevelUp

reduceEnemyHp : ButtleModel -> ButtleModel
reduceEnemyHp model =
  let
    oldEnemyModel = model.enemy
    oldActorModel = model.actor

  in
    { model | 
        enemy =
          { oldEnemyModel | enemyHp = oldEnemyModel.enemyHp - oldActorModel.attack }
    }
    
encountNextEnemy : ButtleModel -> ButtleModel
encountNextEnemy model =
  let
    oldEnemyModel = model.enemy
    oldActorModel = model.actor
    newEnemyModel = 
      { oldEnemyModel |
          enemyHp = oldEnemyModel.lastEnemyHp + 5
          , lastEnemyHp = oldEnemyModel.lastEnemyHp + 5
      }
    newActorModel = 
      { oldActorModel |
        exp = oldActorModel.exp + 10
        , levelFlag = True
      }
  in
  if oldEnemyModel.enemyHp <= 0 then
    { model |
      enemy = newEnemyModel
      , actor = newActorModel
    }
  else
    model

judgeLevelUp : ButtleModel -> ButtleModel
judgeLevelUp model =
  let
    oldActorModel = model.actor
    newActorModel = { oldActorModel | 
                        level = oldActorModel.level + 1
                      , attack = oldActorModel.attack + 5
                      , levelFlag = False
                    }
  in
  if oldActorModel.levelFlag && modBy 30 oldActorModel.exp == 0 then
    { model | actor = newActorModel}
  else
    { model | actor = 
      {oldActorModel | levelFlag = False }
    }

setNewModel : IndexModel -> Navigation.Key -> (Model, Cmd Msg)
setNewModel indexModel navigationKey =
  let
    newModel = initModel navigationKey
    newButtleModel : ButtleModel
    newButtleModel =
      case indexModel.status of
        Just s ->
          s
        Nothing ->
          newModel.buttle
    newTodoModel : List Task
    newTodoModel =
      case indexModel.todos of
        Just t ->
          t
        Nothing ->
          newModel.taskList

    newLoginStatus : LoginStatus
    newLoginStatus =
      case indexModel.loginStatus of
        Just t ->
          Debug.log "status" t
        Nothing ->
          Debug.log "status" initLoginStatus
  in
    ({ newModel | buttle = newButtleModel
                , taskList = List.sortBy .date newTodoModel |> List.reverse
                , loginStatus = newLoginStatus}
      , Cmd.none
    )

-- TODO: ここらへんをかえる

setNewPage : Maybe Routes.Route -> (Model, Cmd Msg) -> ( Model, Cmd Msg )
setNewPage maybeRoute oldModel =
 let
    model = Tuple.first oldModel

    oldLoginDate : LoginStatus
    -- TODO : ここに変更後のModelを渡してやればいけるから、Cmdはその前に実行してやらなきゃならない
    oldLoginDate = Debug.log "check-timing" model.loginStatus
    cmd =
      if oldLoginDate.loginToday == False then
        -- Debug.log "first login" ( Cmd.batch [getLoginDate, setLoginInformation (loginEncoder {oldLoginDate | loginToday = True})])
        Debug.log "first login" ( Cmd.batch [getLoginDate])
      else
        Debug.log "second login" Cmd.none
    -- a = T.millisToPosix model.date
    -- b = Debug.log "time" (TE.posixToParts T.utc a)
    a = Debug.log "checktiming-2" "hoge"
  in
  case maybeRoute of
    Just Routes.Todo ->
      ( { model | page = Todo }, cmd )
    Just Routes.Buttle ->
      ( { model | page = Buttle }, cmd )
    Nothing ->
      ( { model | page = NotFound }, cmd )

getNewId : Cmd Msg
getNewId =
  Random.generate NewId UUID.generator

getDate : Cmd Msg
getDate =
  Ts.perform Tick (Ts.map2 Tuple.pair T.now T.here)

getLoginDate : Cmd Msg
getLoginDate =
  Ts.perform LoginInformation (Ts.map2 Tuple.pair T.now T.here)


deleteTask : Model -> Task -> List Task
deleteTask model task =
  List.filter (\t -> t /= task ) model.taskList


updateChecked : String -> Task -> Task
updateChecked id task =
  if task.id == id then
    { task | checked = not task.checked}
  else
    task

updateTask : Task -> InputType -> String -> Msg
updateTask task msg newTask
  = case msg of
    TaskName ->
      UpdateTask {task | task = newTask}
    Project ->
      UpdateTask {task | project = newTask}
    TaskType ->
      UpdateTask {task | taskType = newTask}
    RepeatTask ->
      UpdateTask {task | repeatTask = newTask}


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case (msg, model.page) of
        (AttackToEnemy, _) ->
          ({model | buttle = attackToEnemy model.buttle}, Cmd.none)
        (NewRoute maybeRoute, _) ->
          setNewPage maybeRoute (model, Cmd.none)
        (Visit (Browser.Internal url), _) ->
          (model, Navigation.pushUrl model.navigationKey (Url.toString url))
        (AddToTask task, _) ->
          let
            newTask = {task | id = model.uid, date = model.loginStatus.loginDate}
          in
          if newTask.id /= "" then
            ( { model | 
                  taskList = (List.append [ newTask ] model.taskList )
                , task = newTask
                , inputWindowViewVisibility = False}
              , Cmd.none)
          else
            ( model, Cmd.none )
        (UpdateTask task, _) ->
          ( { model | task = task }, Cmd.batch [getNewId, getDate] )
        (DeleteTask task, _) ->
          ( {model | taskList = deleteTask model task}, Cmd.none)
        (Tick time, _) ->
          let
            t = Tuple.first time
            zone = Debug.log "timezone" Tuple.second time

            -- TODO: 時間情報は日まででいいので切り捨てる。
            -- TODO: time zoneも保管する → Modelに一時保管かな。Strageには入れなくてよい気がする。
            localTime =
              TE.posixToParts zone t 
              |> TE.partsToPosix zone
              |> T.posixToMillis
          in
          ({ model | loginStatus = {loginDate = localTime, loginToday = True}}, Cmd.none)
        (NewId uuid, _) ->
          ( { model | uid = UUID.toString uuid }, Cmd.none )
        (ChangeChecked task, _) ->
          ( { model | taskList = List.map (updateChecked task.id) model.taskList }, Cmd.none )
        (ShowInputWindow show, _) ->
          ( { model | task = initTask, inputWindowViewVisibility = not show}, Cmd.none  )
        (SelectProjectTab p, _) ->
          ( { model | selectedProject = p}, Cmd.none  )
        (SelectRepeatType typeRep checked, _) ->
          let
            oldTask = model.task
            oldList =
              case typeRep of
                Weekly d ->
                  (model.task.repeatedDay, d)
                Monthly m ->
                  (model.task.repeatedDate, m)

            repList =
              if checked then
                List.filter (\x -> x /= (Tuple.second oldList)) (Tuple.first oldList) 
              else
                List.append [Tuple.second oldList] (Tuple.first oldList)
          in
          case typeRep of
            Weekly _ ->
              ( {model | task = { oldTask | repeatedDay = repList}}, Cmd.none )
            Monthly _ ->
              ( {model | task = { oldTask | repeatedDate = repList}}, Cmd.none )
        (LoginInformation time, _) ->
          let
            t = Tuple.first time
            zone = Debug.log "timezone" Tuple.second time
            localTime =
              TE.posixToParts zone t 
              |> TE.partsToPosix zone
              |> T.posixToMillis
            oldLoginStatus = model.loginStatus
            newloginDate = Debug.log "SetNewPage" {oldLoginStatus | loginDate = localTime}
          in
          ({ model | loginStatus = newloginDate}, 
              Cmd.batch [setLoginInformation (loginEncoder newloginDate)])
        (CloseLoginWindow, _) ->
          let
            oldLoginStatus = Debug.log "CloseLoginWindow" model.loginStatus
            newLoginStatus = {oldLoginStatus | loginToday = True}
          in
          ({model | loginStatus = newLoginStatus },
              Cmd.batch [setLoginInformation (loginEncoder newLoginStatus)])
        _ ->
          Debug.todo "予定外の値が来ていますよ"


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none

-- VIEW

-- VIEW: Login Bounus


-- TODO: ページの体裁を整える
viewLoginStatus : Model -> Html Msg
viewLoginStatus model =
  let 
    loginStatusWindow =
      if not model.loginStatus.loginToday then
        "todo--inputbox--cover"
      else
        "todo--inputbox--none"
    today = T.millisToPosix model.loginStatus.loginDate
            |> TE.posixToParts T.utc

    month =
      if today.month == T.Feb then
        "2"
      else
        "100"
        
  in
  div [ class loginStatusWindow, hidden model.loginStatus.loginToday] 
      [ button [ onClick CloseLoginWindow]
                []
      , h1 [] [ text (month ++ "月" ++ (fromInt today.day) ++ "日")]
      ]

-- VIEW: Buttle
viewEnemy : EnemyModel -> Html Msg
viewEnemy enemyModel =
  div []
      [ text ("Enemy HP: " ++ String.fromInt enemyModel.enemyHp) ]
  

viewActor : ActorModel -> Html Msg
viewActor actorModel =
  div [ class "todo-text" ]
      [ div [] 
            [ text ("LV: " ++ String.fromInt actorModel.level) ]    
      , div [] 
            [ text ("EXP: " ++ String.fromInt actorModel.exp) ]
      , div [] 
            [ text ("Point: " ++ String.fromInt actorModel.point) ]
      , div [] 
            [ text ("Attack: " ++ String.fromInt actorModel.attack) ]
      , button [onClick AttackToEnemy] [ text "Attack" ]        
      ]

viewTodoList : Model -> Html Msg
viewTodoList model =
  div [ class "todo--list" ]
    (List.map viewTodo (List.filter (\x -> x.project == model.selectedProject) model.taskList))

-- VIEW: Todo

viewTodo : Task -> Html Msg
viewTodo todo=
  let
    todoChecked =
      if todo.checked then
        "todo--checked"
      else
        "todo--nochecked"
  in
  div [class "todo--box--single"]
      [ span [ class "todo--space"] [text ""]
      , span [ class todoChecked, onClick (ChangeChecked todo) ] [ text ""]
      , input [id ("todo" ++ todo.task)
        , name "toggle"
        , type_ "checkbox"
        , checked todo.checked
        , onClick (ChangeChecked todo)
        , class "todo--checkbox"
        , hidden True ] [ ]
        , div [ class "todo--container--information"]
              [ label [for ("todo" ++ todo.task), class "todo--task"] [ text todo.task ]
              , div [ class "todo--subrow"]
              [ div [ class "todo--property"] 
                    [ text todo.project ]
              , div [class "todo--property"]
                    [ text todo.taskType]
               ]

        ] 
      ,  label [ onClick (DeleteTask todo), class "todo--delete" ] [ text " [X]" ]      
      ]

-- VIEW : TodoInputWindow

viewListOption : Model -> (Int, String) -> Html Msg
viewListOption model (int, str) =
  let
    optionSelect =
      if model.inputWindowViewVisibility && int == 1 then
        True
      else
        False
  in
  option [selected optionSelect, value str] [ text str ]

viewSelectProject : Model -> Html Msg
viewSelectProject model =
  div []
      [ select
        [ onChange (updateTask model.task Project)]
        (List.map (viewListOption model) projectTypes)
      ]

viewSelectTaskType : Model -> Html Msg
viewSelectTaskType model =
  div []
      [ select
        [ onChange (updateTask model.task TaskType) ]
        (List.map (viewListOption model) tasktypes)
      ]

viewInput : Model -> Html Msg
viewInput model =
  div [ class "todo--input--taskinfo" ]
      [ input
          [ type_ "text"
          , placeholder "やること"
          , autofocus True
          , value model.task.task
          , name "newTodo"
          , onChange (updateTask model.task TaskName)
          -- , onCtrEnter (AddToTask task)
          ]
          []
      , viewSelectProject model
      , viewSelectTaskType model
      ]

viewAddTodo : Task -> Html Msg
viewAddTodo task =
  button [ onClick (AddToTask task) ]
         [ text "Add Task"]

viewCancelTodo : Model -> Html Msg
viewCancelTodo model =
  button [ onClick (ShowInputWindow model.inputWindowViewVisibility) ]
         [ text "Cancel"]

viewRepeatFrequency : Model -> Html Msg
viewRepeatFrequency model =
  let
    repeatTask =
      if model.task.project == "繰り返し" then
        True
      else
        False
    repeatedFrequency = 
      [(1, "-"), (2, "Weekly"), (3, "Monthly")]
  in
  div [ hidden (not repeatTask) ]
      [ select 
        [ onChange (updateTask model.task RepeatTask) ]
        (List.map (viewListOption model) repeatedFrequency)
      , viewRepeatTime model.task
      ]


viewRepeatTime : Task -> Html Msg
viewRepeatTime task =
  let
    weekday = 
      ["月", "火", "水", "木", "金", "土", "日" ]

    monthday =
      ["1", "5", "10", "15", "20", "25", "月末"]

    makeList : TaskRepeatType -> String -> Html Msg
    makeList taskRepType str =
      let
        checkedBox =
          case taskRepType of
            Weekly w 
              -> List.member w task.repeatedDay
            Monthly m
              -> List.member m task.repeatedDate
      in
      div [ id ("todo" ++ str)]
          [ input [ type_ "checkbox"
                  , value str
                  , for ("todo" ++ str)
                  , checked checkedBox
                  , onClick (SelectRepeatType taskRepType checkedBox)
                  ] []
          , label [] [text str]
          ]


    repeatedTime : List (Html Msg)
    repeatedTime =
      case task.repeatTask of
        "Weekly" ->
          List.map (\x -> makeList (Weekly x) x) weekday
        "Monthly" ->
          List.map (\x -> makeList (Monthly x) x) monthday
        "-" ->
          []
        _ ->
          []
  in
  div []
      repeatedTime

viewInputWindow : Model -> Html Msg
viewInputWindow model =
  let 
    todoInputWindow =
      if model.inputWindowViewVisibility then
        "todo--inputbox--cover"
      else
        "todo--inputbox--none"
  in
  div [ class todoInputWindow, hidden (not model.inputWindowViewVisibility) ] 
      [ div [class "todo--inputbox", hidden (not model.inputWindowViewVisibility)]
            [
              viewInput model
            , viewRepeatFrequency model
            , viewAddTodo model.task
            , viewCancelTodo model
            ]
      ]

-- VIEW : FLOAT BUTTON

viewFloatButton : Model -> Html Msg
viewFloatButton model =
  div [class "button--floating", onClick (ShowInputWindow model.inputWindowViewVisibility)]
      []

-- VIEW : CONTAINTS

viewContent : Model -> ( String, Html Msg )
viewContent model =
  case model.page of
    Todo ->
      ( "Todo List"
      , div [class "todo--page"] 
            [ h1 [] [text ("Todo List: " ++ model.selectedProject)]
            , viewLoginStatus model
            , viewInputWindow model
            , lazy viewTodoList model
            , lazy viewFloatButton model
            , viewTodoFooter model
            ]
      )
    Buttle ->
      ( "Buttle Field"
      , div [class "todo--page"]
            [ h1 [] [text "Buttle Field"]
            , div [] 
                  [ viewLoginStatus model
                  , lazy viewEnemy model.buttle.enemy
                  , lazy viewActor model.buttle.actor 
                  ]
            ]
      )
    NotFound ->
      ( "Not Found"
      , div []
            [ h1 [] [text "Page Not Found!!"] ]
      )


-- VIEW : HEADER, FOOTER

viewHeader : Html Msg
viewHeader =
  div []
      [ div []
            [ a [ Routes.href Routes.Todo]
                [ text "todo |"]
            , a [ Routes.href Routes.Buttle]
                [ text " buttle"]
             ]
      ]

viewTodoFooter : Model -> Html Msg
viewTodoFooter model =
  let

    projectTab: String -> (Int, String) -> Html Msg
    projectTab c p =
      div [ class c, onClick (SelectProjectTab (Tuple.second p))] [text (Tuple.second p)]

    showProjectTab: (Int, String) -> Html Msg
    showProjectTab p =
      if Tuple.second p == model.selectedProject then
        projectTab "todo--tab--selected" p
      else
        projectTab "todo--tab--non-selected" p
  in
  div [class "todo--footer"]
      (List.map showProjectTab projectTypes)

viewFooter : Html Msg
viewFooter =
  div []
      [ div []
            [ a [ Routes.href Routes.Todo]
                [ text "todo |"]
            , a [ Routes.href Routes.Buttle]
                [ text " buttle"]
             ]
      ]

-- VIEW : TOTAL

view : Model -> Document Msg
view model =
  let
    ( title, content) =
      viewContent model
  in
  { title = title,
    body = 
      [ div [class "wrap"] [viewHeader, content, viewFooter] ]
  }

-- VIEW : END

-- PORT

port setStatusStorage : Encode.Value -> Cmd msg
port setTasksStorage : Encode.Value -> Cmd msg
port deleteTaskFromDb : Encode.Value -> Cmd msg
port changeCheckedDB : Encode.Value -> Cmd msg
port setLoginInformation : Encode.Value -> Cmd msg

updateWithStorage : Msg -> Model -> ( Model, Cmd Msg )
updateWithStorage msg oldModel =
  let
    ( newModel, cmds ) = update msg oldModel
    newTask = 
      newModel.task
  in
    case msg of
      AddToTask _ ->
        if newTask.id /= "" then
          ( {newModel | task = initTask }
          , Cmd.batch [ setTasksStorage (taskEncoder newTask), cmds]
          )
        else
          ( {newModel | task = initTask }, cmds)
      AttackToEnemy ->
        ( newModel
        , Cmd.batch [ setStatusStorage (statusEncoder newModel.buttle), cmds]
        )
      DeleteTask task ->
        ( newModel
        , Cmd.batch [ deleteTaskFromDb (taskEncoder task) , cmds] )
      ChangeChecked task ->
        ( newModel
        , Cmd.batch [ changeCheckedDB (taskEncoder task), cmds] )
      _ ->
        ( newModel, cmds )

-- JSON ENCODE/DECODE
-- Buttle
statusEncoder : ButtleModel -> Encode.Value
statusEncoder buttle =
  Encode.object
    [ ("enemy", Encode.object 
        [ ("id", Encode.int buttle.enemy.id)
        , ("enemyHp", Encode.int buttle.enemy.enemyHp)
        , ("lastEnemyHp", Encode.int buttle.enemy.lastEnemyHp)
        ]
      )
    , ("actor", Encode.object 
        [ ("id", Encode.int buttle.actor.id)
        , ("exp", Encode.int buttle.actor.exp)
        , ("level", Encode.int buttle.actor.level)
        , ("levelFlag", Encode.bool buttle.actor.levelFlag)
        , ("point", Encode.int buttle.actor.point)
        , ("attack", Encode.int buttle.actor.attack)
        ]
      )
    ]

statusEnemyDecoder : Decode.Decoder EnemyModel
statusEnemyDecoder =
  Decode.succeed EnemyModel
    |> DP.required "id" Decode.int
    |> DP.required "enemyHp" Decode.int
    |> DP.required "lastEnemyHp" Decode.int

statusActorDecoder : Decode.Decoder ActorModel
statusActorDecoder =
  Decode.succeed ActorModel
    |> DP.required "id" Decode.int
    |> DP.required "exp" Decode.int
    |> DP.required "level" Decode.int
    |> DP.required "levelFlag" Decode.bool
    |> DP.required "point" Decode.int
    |> DP.required "attack" Decode.int

statusDecoder : Decode.Decoder ButtleModel
statusDecoder =
  Decode.map2 ButtleModel
    (Decode.field "enemy" statusEnemyDecoder)
    (Decode.field "actor" statusActorDecoder)

-- Todo

taskEncoder : Task -> Encode.Value
taskEncoder task =
  Encode.object
    [ ("id", Encode.string task.id)
    , ("date", Encode.int task.date)
    , ("checked", Encode.bool task.checked)
    , ("task", Encode.string task.task)
    , ("project", Encode.string task.project)
    , ("taskType", Encode.string task.taskType)
    , ("repeatTask", Encode.string task.repeatTask)
    , ("repeatedDay", Encode.list Encode.string task.repeatedDay)
    , ("repeatedDate", Encode.list Encode.string task.repeatedDate)
    ]

taskDecoder : Decode.Decoder Task
taskDecoder =
  Decode.succeed Task
    |> DP.required "id" Decode.string
    |> DP.required "date" Decode.int
    |> DP.required "checked" Decode.bool
    |> DP.required "task" Decode.string
    |> DP.required "project" Decode.string
    |> DP.required "taskType" Decode.string
    |> DP.required "repeatTask" Decode.string
    |> DP.required "repeatedDay" (Decode.list Decode.string)
    |> DP.required "repeatedDate" (Decode.list Decode.string)

-- Login: Login Event



loginEncoder : LoginStatus -> Encode.Value
loginEncoder loginStatus =
  Encode.object
    [ ("loginDate", Encode.int loginStatus.loginDate)
    , ("loginToday", Encode.bool loginStatus.loginToday)
    ]

loginDecoder : Decode.Decoder LoginStatus
loginDecoder =
  Decode.succeed LoginStatus
    |> DP.required "loginToday" Decode.bool
    |> DP.required "loginDate" Decode.int


indexDecoder : Decode.Decoder IndexModel
indexDecoder =
 Decode.map3 IndexModel
   (Decode.field "status" <| Decode.nullable statusDecoder)
   (Decode.field "todos" <| Decode.nullable (Decode.list taskDecoder))
   (Decode.field "loginStatus" <| Decode.nullable loginDecoder)
