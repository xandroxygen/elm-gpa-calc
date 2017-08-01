import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Dict exposing (Dict, get)

main =
  Html.beginnerProgram { model = model, view = view, update = update }

-- MODEL

type alias Scale = Dict String Float

type alias Class =
  { name: String
  , hours: Float
  , grade: String
  }

addClass: Class -> List Class -> List Class
addClass class list =
  class :: list

type alias Model =
  { classes: List Class
  , initialPoints: Float
  , initialHours: Float
  , projectedGPA: Float
  , currentClassName: String
  , currentClassHours: Float
  , currentClassGrade: String
  , scale: Scale
  }

getPoints: Scale -> Class -> Float
getPoints scale class =
  case get class.grade scale of
    Nothing ->
      0.0
    Just val ->
      val * class.hours

updateGPA: Model -> Model
updateGPA model =
  let
    hours = List.map .hours model.classes
    points = List.map (getPoints model.scale) model.classes

    totalHours = List.foldr (+) model.initialHours hours
    totalPoints = List.foldr (+) model.initialPoints points
  in
    { model | projectedGPA = totalPoints / totalHours }

resetCurrentClass: Model -> Model
resetCurrentClass model =
  { model | currentClassName = ""
          , currentClassHours = 0.0
          , currentClassGrade = "" }

model: Model
model =
  { classes = []
  , initialPoints = 0.0
  , initialHours = 0
  , projectedGPA = 0.00
  , currentClassName = ""
  , currentClassHours = 0.0
  , currentClassGrade = ""
  , scale = Dict.fromList
    [ ("A", 4.0)
    , ("A-", 3.7)
    , ("B+", 3.4)
    , ("B", 3.0)
    , ("B-", 2.7)
    , ("C+", 2.4)
    , ("C", 2.0)
    , ("C-", 1.7)
    , ("D+", 1.4)
    , ("D", 1.0)
    , ("D-", 0.7)
    , ("E", 0.0)
    ]
  }


-- UPDATE

type Msg
  = AddClass
  | InitialHours String
  | InitialPoints String
  | ClassName String
  | ClassHours String
  | ClassGrade String

update: Msg -> Model -> Model
update msg model =
  case msg of
    AddClass ->
      let
        class =
          { name = model.currentClassName
          , hours = model.currentClassHours
          , grade = model.currentClassGrade
          }
      in
        { model | classes = addClass class model.classes }
        |> updateGPA
        |> resetCurrentClass

    InitialHours hours ->
      { model | initialHours = Result.withDefault 0 (String.toFloat hours) }
      |> updateGPA

    InitialPoints points ->
      { model | initialPoints = Result.withDefault 0 (String.toFloat points) }
      |> updateGPA

    ClassName name ->
      { model | currentClassName = name }

    ClassHours hours ->
      { model | currentClassHours = Result.withDefault 0 (String.toFloat hours) }

    ClassGrade grade ->
      { model | currentClassGrade = grade }

-- VIEW

view: Model -> Html Msg
view model =
  div []
    [ input [ type_ "text", placeholder "Initial credit hours", onInput InitialHours ] []
    , input [ type_ "text", placeholder "Initial points", onInput InitialPoints ] []
    , newClass model
    , ul [] (List.map viewClass model.classes)
    , div [] [ text (toString model.projectedGPA) ]
    ]

newClass: Model -> Html Msg
newClass model =
  div []
    [ input [ type_ "text", placeholder "Class Name", onInput ClassName] []
    , input [ type_ "text", placeholder "Class Hours", onInput ClassHours ] []
    , input [ type_ "text", placeholder "Class Grade", onInput ClassGrade] []
    , button [ onClick AddClass ] [ text "Add Class" ]
    ]

viewClass: Class -> Html Msg
viewClass class =
  li []
    [ text class.name
    , text class.grade
    , text (toString class.hours)
    ]

