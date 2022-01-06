port module TodoMain exposing ( main
                          , initTodoModel
                          , reduceEnemyHp
                          , encountNextEnemy
                          , judgeLevelUp )
import Routes
import Url exposing (Url)

import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Navigation
import Html exposing ( Html, div, text, h1, a, input, label, button, label )
import Html.Attributes exposing ( class, type_, name, for, value )
import Html.Events exposing ( .. )
import Html.Events.Extra exposing ( onChange )
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode
import Html.Attributes exposing (placeholder)
import Html.Attributes exposing (autofocus)

-- MAIN
main : Program Encode.Value Model Msg
main = Browser.application
  { init = init
  , view = view
  , update = updateWithStorage
  , subscriptions = \_ -> Sub.none
  , onUrlRequest = Visit
  , onUrlChange = Routes.match >> NewRoute
  }


-- MODEL

type Page
  = Todo
  | Buttle
  | NotFound

type alias Task =
  { id : Int
  , checked : Bool
  , task : String
  , project : String
  , todoType : String
  }

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


type alias Model =
  { page : Page
  , navigationKey : Navigation.Key
  , buttle : ButtleModel
  , todos : List Task
  , todo : String
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
  [ ]


initTask : Task
initTask =
  { id = 0
  , checked = False
  , task = ""
  , project = "" 
  , todoType = ""
  }


initModel : Navigation.Key -> Model
initModel navigationKey =
  { page = NotFound
  , navigationKey = navigationKey
  , buttle = initButtleModel
  , todos = initTodoModel
  , todo = ""
  }


init : Encode.Value -> Url -> Navigation.Key -> (Model, Cmd Msg)
init flags url navigationKey = 
  case Decode.decodeValue statusDecoder flags of
    Ok model -> setButtleModel model navigationKey 
                |> setNewPage (Routes.match url)
    Err _ -> setNewPage (Routes.match url) (initModel navigationKey)

  
-- UPDATE
type Msg 
  = AttackToEnemy
  | NewRoute (Maybe Routes.Route)
  | Visit UrlRequest
  | AddToTask Task
  | NewTask String

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

setButtleModel : ButtleModel -> Navigation.Key -> Model
setButtleModel buttleModel navigationKey =
  let
    newModel = initModel navigationKey 
  in
    { newModel | buttle = buttleModel }


setNewPage : Maybe Routes.Route -> Model -> ( Model, Cmd Msg )
setNewPage maybeRoute model =
  case maybeRoute of
    Just Routes.Todo ->
      ( { model | page = Todo }, Cmd.none )
    Just Routes.Buttle ->
      ( { model | page = Buttle }, Cmd.none )
    Nothing ->
      ( { model | page = NotFound }, Cmd.none )


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case (msg, model.page) of
        (AttackToEnemy, _) ->
          ({model | buttle = attackToEnemy model.buttle}, Cmd.none)
        (NewRoute maybeRoute, _) ->
          setNewPage maybeRoute model
        (Visit (Browser.Internal url), _) ->
          (model, Navigation.pushUrl model.navigationKey (Url.toString url))
        (AddToTask task, _) ->
          ({ model | todos = (List.append model.todos [task]), todo = ""}, Cmd.none)
        (NewTask task, _) ->
          ({ model | todo = task}, Cmd.none)
        _ ->
          (model, Cmd.none)

-- VIEW

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

viewTodoList : List Task -> Html Msg
viewTodoList model =
  div []
    (List.map viewTodo model)

viewTodo : Task -> Html msg
viewTodo todo =
  div []
      [
        input [name "toggle", type_ "checkbox"] [ ]
      , label [for "toggle"] [ text todo.task ]
      ]

viewInput : String -> Html Msg
viewInput task =
  div []
      [ input
          [ type_ "text"
          , placeholder "やること"
          , autofocus True
          , value task
          , name "newTodo"
          , onChange NewTask
          ]
          []
      ]

viewAddTodo : Task -> Html Msg
viewAddTodo task =
  button [ onClick (AddToTask task) ]
        [ text "Add Task"]


viewContent : Model -> ( String, Html Msg )
viewContent model =
  case model.page of
    Todo ->
      ( "Todo List"
      , div [] 
            [ h1 [] [text "Todo List"]
            , viewInput model.todo 
            , viewAddTodo { initTask | task = model.todo }
            , viewTodoList model.todos
            ]
      )
    Buttle ->
      ( "Buttle Field"
      , div []
            [ h1 [] [text "Buttle Field"]
            , div [] 
                  [ viewEnemy model.buttle.enemy
                  , viewActor model.buttle.actor 
                  ]
            ]
      )
    NotFound ->
      ( "Not Found"
      , div []
            [ h1 [] [text "Page Not Found!!"] ]
      )

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

view : Model -> Document Msg
view model =
  let
    ( title, content) =
      viewContent model
  in
  { title = title,
    body = 
      [ viewHeader, content ]
  }

-- PORT

port setStorage : Encode.Value -> Cmd msg


updateWithStorage : Msg -> Model -> ( Model, Cmd Msg )
updateWithStorage msg oldModel =
  let
    ( newModel, cmds ) = update msg oldModel
  in
    ( newModel
    , Cmd.batch [ setStorage (statusEncoder newModel), cmds]
    )

-- JSON ENCODE/DECODE

statusEncoder : Model -> Encode.Value
statusEncoder model =
  Encode.object
    [ ("enemy", Encode.object 
        [ ("id", Encode.int model.buttle.enemy.id)
        , ("enemyHp", Encode.int model.buttle.enemy.enemyHp)
        , ("lastEnemyHp", Encode.int model.buttle.enemy.lastEnemyHp)
        ]
      )
    , ("actor", Encode.object 
        [ ("id", Encode.int model.buttle.actor.id)
        , ("exp", Encode.int model.buttle.actor.exp)
        , ("level", Encode.int model.buttle.actor.level)
        , ("levelFlag", Encode.bool model.buttle.actor.levelFlag)
        , ("point", Encode.int model.buttle.actor.point)
        , ("attack", Encode.int model.buttle.actor.attack)
        ]
      )
    ]

statusEnemyDecoder : Decode.Decoder EnemyModel
statusEnemyDecoder =
  Decode.succeed EnemyModel
    |> required "id" Decode.int
    |> required "enemyHp" Decode.int
    |> required "lastEnemyHp" Decode.int

statusActorDecoder : Decode.Decoder ActorModel
statusActorDecoder =
  Decode.succeed ActorModel
    |> required "id" Decode.int
    |> required "exp" Decode.int
    |> required "level" Decode.int
    |> required "levelFlag" Decode.bool
    |> required "point" Decode.int
    |> required "attack" Decode.int

statusDecoder : Decode.Decoder ButtleModel
statusDecoder =
  Decode.map2 ButtleModel
    (Decode.field "enemy" statusEnemyDecoder)
    (Decode.field "actor" statusActorDecoder)
