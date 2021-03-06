port module TodoMain exposing ( main
                          , initTodoModel
                          , setLoginInformation)

import Browser exposing (Document)
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
main = Browser.document
  { init = init
  , view = view
  , update = updateWithStorage
  , subscriptions = subscriptions
  }

-- MODEL

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
  [ (0, "今日")
  , (1, "メイン")
  , (2, "サブ")
  , (3, "繰り返し")
  , (4, "アーカイブ")
  ]

tasktypes : List (Int, String)
tasktypes =
  [ (1, "次の行動")
  , (2, "連絡待ち")
  , (3, "待機")
  ]

type alias IndexModel =
  { todos : Maybe (List Task)
  , loginStatus : Maybe LoginStatus
  }

type alias LoginStatus =
  { loginToday : Bool
  , loginDate : Int
  }

type alias Model =
  { taskList : List Task
  , task : Task
  , uid : String
  , loginStatus : LoginStatus
  , timeZone : T.Zone
  , inputWindowViewVisibility : Bool
  , selectedProject : String
  }


initTodoModel : List Task
initTodoModel =
  []


initTask : Task
initTask =
  let
    firstProject = 
      case List.head (List.drop 1 projectTypes) of
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

initModel : Model
initModel =
  let
    initProject =
      case List.head projectTypes of
        Just p -> p
        Nothing -> (0, "Not Found") 
  in
  { taskList = initTodoModel
  , task = initTask
  , uid = ""
  , inputWindowViewVisibility = False
  , selectedProject = Tuple.second initProject
  , loginStatus = initLoginStatus
  , timeZone = T.utc
  }


init : Encode.Value -> (Model, Cmd Msg)
init flags =
  case Decode.decodeValue indexDecoder flags of
  -- TODO あとでなおす
    Ok model -> setNewModel model
    Err _ -> (initModel, Cmd.none)

  
-- UPDATE
type Msg 
  = AddToTask Task
  | DeleteTask Task
  | UpdateTask Task
  | ArchiveCheckedTasks
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
setNewModel : IndexModel -> (Model, Cmd Msg)
setNewModel indexModel =
  let
    newModel = initModel
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
          -- Debug.log "status" t
          t
        Nothing ->
          -- Debug.log "status" initLoginStatus
          initLoginStatus
  in
    ({ newModel | taskList = List.sortBy .date newTodoModel |> List.reverse
                , loginStatus = newLoginStatus}
      , Cmd.none
    )



setNewPage : (Model, Cmd Msg) -> ( Model, Cmd Msg )
setNewPage oldModel =
 let
    model = Tuple.first oldModel

    cmd =
      -- if oldLoginDate.loginToday == False then
--        Debug.log "first login" ( Cmd.batch [getLoginDate])
        Cmd.batch [getLoginDate]
      -- else
      --   Debug.log "second login" Cmd.none
  in
    ( model , cmd )

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
    case (msg, model) of
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
        (ArchiveCheckedTasks, _) ->
          let
            newTasks =
              List.map setArchive model.taskList

            setArchive task =
              if task.checked then
                {task | project = "アーカイブ"}
              else
                task
          in
          ( {model | taskList = newTasks}, Cmd.none )
        (Tick time, _) ->
          let
            t = Tuple.first time
--            zone = Debug.log "timezone" Tuple.second time
            zone = Tuple.second time

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
                |> TE.add TE.Day 0 zone

-- TODO: 日付調整を入れているので、本番の時はけすようにする
--            zone = Debug.log "timezone" Tuple.second time
            zone = Tuple.second time
            localTime =
              TE.floor TE.Day zone t
              |> T.posixToMillis

            oldLoginStatus = model.loginStatus

--            logind = Debug.log "login" (oldLoginStatus.loginDate == localTime)
            logind = oldLoginStatus.loginDate == localTime
            
            newloginDate = {oldLoginStatus | loginDate = localTime, loginToday = logind}

            todaysWeekDay : String
            todaysWeekDay =
              toJapaneseWeekday (T.toWeekday zone <| T.millisToPosix localTime)

            todaysDate : String
            todaysDate =

              let
                ti =
                    T.millisToPosix localTime
                dif =
                  -- Debug.log "月末判定" 
                  --   (
                    ti == TE.add TE.Day 1 zone ti
                    -- )
              in
              if dif then
                T.toDay zone ti
                |> fromInt              
              else
                "月末"

            weeklyNewList : List Task
            weeklyNewList =
              List.filter (\x -> x.repeatTask == "Weekly") model.taskList
              |> List.filter (\x -> List.member todaysWeekDay x.repeatedDay ) 


            monthlyNewList =
              List.filter (\x -> x.repeatTask == "Monthly") model.taskList
              |> List.filter (\x -> List.member todaysDate x.repeatedDate)

            todaysList : List Task
            todaysList =
              List.map (\x -> {x | id = x.id ++ "R",date = 0,repeatTask="-", project = "今日"}) (List.append weeklyNewList monthlyNewList)
            
            oldTodaysList : List Task
            oldTodaysList =  
              List.filter (\x -> x.project  == "今日") model.taskList
            judgeId : List Task -> Task -> Bool
            judgeId taskList task =
              List.map (\x -> 
                          if x.id /= task.id then
                            True
                          else
                            False) taskList
              |> List.member False

            nextList =
              List.filter (\x -> not (judgeId oldTodaysList x)) todaysList
              |> List.append model.taskList

          in
          if logind then
          (model, Cmd.none)
          else
          ({ model | taskList = nextList, loginStatus = newloginDate, timeZone = zone}, 
              Cmd.none)
        (CloseLoginWindow, _) ->
          let
            oldLoginStatus = model.loginStatus
            newLoginStatus = {oldLoginStatus | loginToday = True}
            -- newLoginStatus = {oldLoginStatus | loginToday = False}
          in
          ({model | loginStatus = newLoginStatus },
              Cmd.batch [setLoginInformation (loginEncoder newLoginStatus)])
        -- _ ->
        -- -- TODO: エラーを拾うようにはなっていない
        --   (model, Cmd.none)

-- UPDATE: Tool

toJapaneseWeekday : T.Weekday -> String
toJapaneseWeekday weekday =
  case weekday of
    T.Mon -> "月"
    T.Tue -> "火"
    T.Wed -> "水"
    T.Thu -> "木"
    T.Fri -> "金"
    T.Sat -> "土"
    T.Sun -> "日"

toJapaneseMonth : T.Month -> String
toJapaneseMonth month =
  case month of
    T.Jan -> "1"
    T.Feb -> "2"
    T.Mar -> "3"
    T.Apr -> "4"
    T.May -> "5"
    T.Jun -> "6"
    T.Jul -> "7"
    T.Aug -> "8"
    T.Sep -> "9"
    T.Oct -> "10"
    T.Nov -> "11"
    T.Dec -> "12"

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
            |> TE.posixToParts model.timeZone

    month =
      toJapaneseMonth today.month

    todaysTasks =
      List.filter (\x -> x.project == "今日") model.taskList
        
  in
  div [ class loginStatusWindow, hidden model.loginStatus.loginToday] 
      [ div [class "todo--inputbox", hidden model.loginStatus.loginToday]
            [
              h2 [] [ text (month ++ "月" ++ (fromInt today.day) ++ "日のクエスト")]
            , h3 [] [ text "繰り返しクエストの登録漏れに注意して！！"]
            , div []
                  (List.map (\x -> div [] [text x.task ]) todaysTasks)
            , button [ onClick CloseLoginWindow]
                  [ text "りょうかい!!"]
            ] 
      ]

-- VIEW: Buttle

viewTodoList : Model -> Html Msg
viewTodoList model =
  div [ class "todo--list" ]
    (List.map viewTodo (List.filter (\x -> x.project == model.selectedProject) model.taskList))

-- VIEW: Todo
-- TODO: 作業中！！

viewTodoHeadline : Model -> Html Msg
viewTodoHeadline model =
  div []
      [ h1 [] [text ("Todo List: " ++ model.selectedProject)]
      , button [onClick ArchiveCheckedTasks ] [text "Quest Clear!!"]
      ]
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
      ,  label [ onClick (DeleteTask todo), class "todo--delete" ] [ ]      
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

-- TODO: 位置調整
viewFloatButton : Model -> Html Msg
viewFloatButton model =
  div [class "button--floating", onClick (ShowInputWindow model.inputWindowViewVisibility)]
      []

-- VIEW : CONTAINTS

viewContent : Model -> ( String, Html Msg )
viewContent model =
    ( "Todo List"
    , div [class "todo--page"] 
          [ viewTodoHeadline model
          , viewLoginStatus model
          , viewInputWindow model
          , lazy viewTodoList model
          , lazy viewFloatButton model
          , viewTodoFooter model
          ]
    )


-- VIEW : HEADER, FOOTER

viewHeader : Html Msg
viewHeader =
  div []
      [
         text "header"
      ]

viewTodoFooter : Model -> Html Msg
viewTodoFooter model =
  let
    newProjectTypes = 
      List.take 5 projectTypes
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
      (List.map showProjectTab newProjectTypes)

viewFooter : Html Msg
viewFooter =
  div []
      [
         text "footer"
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
      ArchiveCheckedTasks ->
          ( newModel, Cmd.batch (List.map (\task -> setTasksStorage (taskEncoder task)) newModel.taskList) )
      DeleteTask task ->
        ( newModel
        , Cmd.batch [ deleteTaskFromDb (taskEncoder task) , cmds] )
      ChangeChecked task ->
        ( newModel
        , Cmd.batch [ changeCheckedDB (taskEncoder task), cmds] )
      LoginInformation _ ->
        ( newModel, Cmd.batch (List.map (\task -> setTasksStorage (taskEncoder task)) newModel.taskList) )
      _ ->
        ( newModel, cmds )

-- JSON ENCODE/DECODE

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
 Decode.map2 IndexModel
   (Decode.field "todos" <| Decode.nullable (Decode.list taskDecoder))
   (Decode.field "loginStatus" <| Decode.nullable loginDecoder)
