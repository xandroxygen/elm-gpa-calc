import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Dict exposing (Dict, get)
import Round exposing (round)
import Json.Decode as Json

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

    gpa =
      if totalHours == 0 then
        0.0
      else
        totalPoints / totalHours
  in
    { model | projectedGPA = gpa }

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
  | RemoveClass Int
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

      RemoveClass index ->
        let
            before =
              List.take index model.classes

            after =
              List.drop (index + 1) model.classes

            newClasses =
              before ++ after
        in
          {model | classes = newClasses }
          |> updateGPA

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
                | grade = String.toUpper grade
              }
          }

-- VIEW

view: Model -> Html Msg
view model =
  div [ class "container" ]
    [ node "link" [ rel "stylesheet", href "style.css" ] []
    , node "link" [ rel "stylesheet", type_ "text/css", href "//fonts.googleapis.com/css?family=Raleway:400,300,600"] []
    , heading model
    , initialData model
    , classInfo model
    , newClass model
    , classTable model
    , projectedGPA model
    ]

heading: Model -> Html msg
heading model =
  div [ class "heading row" ]
    [
      div [ class "offset-by-three six columns"]
        [ h1 [ class "centered" ] [ text "GPA Calculator" ]
        , h6 [ class "centered" ] [ text "A simple cumulative GPA calculator, built entirely in Elm. The GPA calculator is my personal \"Hello World\" that lets me experience the language enough to enjoy it. CSS provided by Skeleton."]
        ]
    ]
initialData: Model -> Html Msg
initialData model =
  div [ class "row" ]
    [  step 1
    ,  description "(Optional) Add your current points and credit hours. These are usually found on your transcript or class record."
    , div [ class "six columns" ]
      [ textInput "Initial Credit Hours" InitialHours (toEmptyString model.initialHours)
      , textInput "Initial Points" InitialPoints (toEmptyString model.initialPoints)
      ]
    ]

classInfo: Model -> Html Msg
classInfo model =
  div [ class "row" ]
    [ step 2
    , description "Add desired classes - the name is optional, but hours and a standard letter grade are required."
    ]

newClass: Model -> Html Msg
newClass model =
  div [ class "row" ]
    [
      div [ class "offset-by-two columns" ]
        [ textInput "Class Name" ClassName model.currentClass.name
        , textInput "Class Hours" ClassHours (toEmptyString model.currentClass.hours)
        , textInput "Class Grade" ClassGrade model.currentClass.grade
        , button [ onClick AddClass, class "button-primary" ] [ text "Add Class" ]
        ]
    ]

classTable: Model -> Html Msg
classTable model =
  div [ class "row" ]
    [
      div [ class "offset-by-two ten columns"]
       [
        ol [] (List.indexedMap viewClass model.classes)
       ]
    ]

viewClass: Int -> Class -> Html Msg
viewClass index currentClass =
  li []
    [ readOnlyInput currentClass.name
    , readOnlyInput (Round.round 1 currentClass.hours)
    , readOnlyInput currentClass.grade
    , button [ onClick (RemoveClass index) ] [ text "Remove" ]
    ]

projectedGPA: Model -> Html Msg
projectedGPA model =
  div [ class "row" ]
    [ step 3
    , description "Check your projected GPA (and try not to freak out about the classes where you need a better grade)."
    , div [ class "six columns" ] [ h2 [] [ text (Round.round 2 model.projectedGPA) ] ]
    ]

-- helper view functions

step: Int -> Html Msg
step num =
  div [ class "two columns" ] [ h4 [] [ text ("Step " ++ (toString num) ++ ":") ] ]

description: String -> Html Msg
description blurb =
  div [ class "four columns" ] [ text blurb ]

readOnlyInput: String -> Html Msg
readOnlyInput v =
  input [ class "class-info", type_ "text", value v, disabled True ] []

textInput: String -> (String -> Msg) -> String -> Html Msg
textInput name msg v  =
  input [ type_ "text" , placeholder name, onBlurWithTargetValue msg, value v, class "text-input" ] []

onBlurWithTargetValue : (String -> msg) -> Attribute msg
onBlurWithTargetValue tagger =
    on "blur" (Json.map tagger targetValue)

toEmptyString: Float -> String
toEmptyString float =
  if float == 0.0
  then ""
  else toString float