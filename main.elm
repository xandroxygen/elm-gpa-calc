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

type alias Model =
  { classes: List Class
  , initialPoints: Float
  , initialHours: Float
  , projectedGPA: Float
  , currentClass: Class
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
    hours =
      List.map .hours model.classes

    points =
      List.map (getPoints model.scale) model.classes

    totalHours =
      List.foldr (+) model.initialHours hours

    totalPoints =
      List.foldr (+) model.initialPoints points
  in
    { model | projectedGPA = totalPoints / totalHours }

resetCurrentClass: Model -> Model
resetCurrentClass model =
  { model | currentClass = (Class "" 0.0 "") }

model: Model
model =
  { classes = []
  , initialPoints = 0.0
  , initialHours = 0
  , projectedGPA = 0.00
  , currentClass = (Class "" 0.0 "")
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
  let
      { currentClass } = model
  in
    case msg of
      AddClass ->
          { model | classes = model.currentClass :: model.classes }
          |> updateGPA
          |> resetCurrentClass

      InitialHours hours ->
        { model | initialHours = Result.withDefault 0 (String.toFloat hours) }
        |> updateGPA

      InitialPoints points ->
        { model | initialPoints = Result.withDefault 0 (String.toFloat points) }
        |> updateGPA

      ClassName name ->
          { model
            | currentClass =
              { currentClass
                | name = name
              }
          }

      ClassHours hours ->
          { model
            | currentClass =
              { currentClass
                | hours = Result.withDefault 0 (String.toFloat hours)
              }
          }

      ClassGrade grade ->
          { model
            | currentClass =
              { currentClass
                | grade = grade
              }
          }

-- VIEW

view: Model -> Html Msg
view model =
  div []
    [ initialData model
    , newClass model
    , ul [] (List.map viewClass model.classes)
    , div [] [ text (toString model.projectedGPA) ]
    ]

initialData: Model -> Html Msg
initialData model =
  div []
    [ textInput "Initial Credit Hours" InitialHours (toEmptyString model.initialHours)
    , textInput "Initial Points" InitialPoints (toEmptyString model.initialPoints)
    ]

newClass: Model -> Html Msg
newClass model =
  div []
    [ textInput "Class Name" ClassName model.currentClass.name
    , textInput "Class Hours" ClassHours (toEmptyString model.currentClass.hours)
    , textInput "Class Grade" ClassGrade model.currentClass.grade
    , button [ onClick AddClass ] [ text "Add Class" ]
    ]

viewClass: Class -> Html Msg
viewClass class =
  li []
    [ div [] [ text class.name ]
    , div [] [ text class.grade ]
    , div [] [ text (toString class.hours) ]
    ]

textInput: String -> (String -> Msg) -> String -> Html Msg
textInput name msg v  =
  input [ type_ "text", placeholder name, onInput msg, value v ] []

toEmptyString: Float -> String
toEmptyString float =
  if float == 0.0
  then ""
  else toString float