import Html exposing (..)
import Html.Events exposing (onClick)
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
  , scale: Scale
  }

getPoints: Scale -> String -> Float
getPoints scale grade =
  case get grade scale of
    Nothing ->
      0.0
    Just val ->
      val

updateGPA: Model -> Model
updateGPA model =
  let
    hours = List.map .hours model.classes
    totalHours = List.foldr (+) model.initialHours hours
    grades = List.map .grade model.classes
    -- map grades to points
    getPoints2 = getPoints model.scale
    points = List.map getPoints2 grades

    totalPoints = List.foldr (+) model.initialPoints points
    projectedGPA = totalHours / totalPoints
  in
    { model | projectedGPA = projectedGPA }

model: Model
model =
  { classes = []
  , initialPoints = 0.0
  , initialHours = 0
  , projectedGPA = 0.00
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
  = Add Class
  | InitialHours Float
  | InitialPoints Float

update: Msg -> Model -> Model
update msg model =
  case msg of
    Add class ->
      { model | classes = addClass class model.classes }
      |> updateGPA

    InitialHours hours ->
      { model | initialHours = hours }
      |> updateGPA

    InitialPoints points ->
      { model | initialPoints = points }
      |> updateGPA

-- VIEW

view: Model -> Html Msg
view model =
  div [] []
