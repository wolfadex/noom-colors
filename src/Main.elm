module Main exposing (Dairy, Flags, FoodStyle, Model, Msg, main)

import Browser exposing (Document)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Fireworks
import Random
import Update.Pipeline
import Validator exposing (Validator)


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { caloriesPerServing : String
    , gramsPerServing : String
    , foodStyle : FoodStyle
    , dairy : Maybe Dairy
    , previousFoodColorIsGreen : Bool
    , fireworks : Fireworks.Model
    }


type FoodStyle
    = Solid WholeGrain
    | Liquid LiquidType
    | Soup


type WholeGrain
    = WholeGrain
    | NotWholeGrain


type LiquidType
    = ArtificialSweetners
    | Soda
    | Alcohol
    | RegularLiquid


type Dairy
    = NotDairy
    | NonFat
    | LowFat
    | WholeFat


type FoodColor
    = Green
    | Yellow
    | Red


type alias Flags =
    { initialSeed : Int
    , windowWidth : Float
    , windowHeight : Float
    }


init : Flags -> ( Model, Cmd Msg )
init { initialSeed, windowWidth, windowHeight } =
    ( { caloriesPerServing = "0"
      , gramsPerServing = "0"
      , foodStyle = Solid NotWholeGrain
      , dairy = Nothing
      , fireworks =
            Fireworks.init
                { initialSeed = Random.initialSeed initialSeed
                , windowWidth = floor windowWidth
                , windowHeight = floor windowHeight
                }
      , previousFoodColorIsGreen = False
      }
    , Cmd.none
    )


type Msg
    = SetCaloriesPerServing String
    | SetGramsPerServing String
    | SetDairy Dairy
    | SetFoodStyle FoodStyle
    | FireworksMessage Fireworks.Msg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map FireworksMessage (Fireworks.subscriptions model.fireworks)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    (case msg of
        SetCaloriesPerServing caloriesPerServing ->
            { model | caloriesPerServing = caloriesPerServing }

        SetGramsPerServing gramsPerServing ->
            { model | gramsPerServing = gramsPerServing }

        SetDairy dairyFat ->
            { model | dairy = Just dairyFat }

        SetFoodStyle foodStyle ->
            { model | foodStyle = foodStyle }

        FireworksMessage message ->
            { model | fireworks = Fireworks.update message model.fireworks }
    )
        |> triggerFireworks
        |> Update.Pipeline.save


triggerFireworks : Model -> Model
triggerFireworks model =
    let
        thisFoodColorIsGreen : Bool
        thisFoodColorIsGreen =
            case calculateFoodColor model of
                Just Green ->
                    True

                _ ->
                    False
    in
    { model
        | previousFoodColorIsGreen = thisFoodColorIsGreen
        , fireworks =
            if not model.previousFoodColorIsGreen && thisFoodColorIsGreen then
                Fireworks.burst model.fireworks

            else
                model.fireworks
    }


calorieValidator : Validator { a | caloriesPerServing : String } String Int
calorieValidator =
    Validator.succeed identity
        |> Validator.required .caloriesPerServing String.isEmpty "Calories per serving is required" (Validator.custom realFromString)


gramsValidator : Validator { a | gramsPerServing : String } String Int
gramsValidator =
    Validator.succeed identity
        |> Validator.required .gramsPerServing String.isEmpty "Grams per serving is required" (Validator.custom realFromString)


realFromString : String -> Result (List String) Int
realFromString str =
    case String.toInt (String.trim str) of
        Nothing ->
            Err [ "Must be a positive, whole number" ]

        Just i ->
            if i > 0 then
                Ok i

            else
                Err [ "Must be greater than 0" ]


view : Model -> Document Msg
view model =
    { title = "Noom Colors"
    , body =
        [ layout
            [ width fill
            , height fill
            , Fireworks.view model.fireworks
                |> behindContent
            ]
            (viewModel model)
        ]
    }


viewModel : Model -> Element Msg
viewModel model =
    column
        [ padding 16
        , spacing 16
        , centerX
        ]
        [ paragraph []
            [ text "Your food is: "
            , case calculateFoodColor model of
                Nothing ->
                    text "Unknown"

                Just color ->
                    foodColorToString color
                        |> text
                        |> el [ Font.color (foodColorToColor color) ]
            ]
        , dairySelection model.dairy
        , case model.dairy of
            Just NotDairy ->
                viewNotDairy model

            _ ->
                none
        ]


foodColorToColor : FoodColor -> Color
foodColorToColor color =
    case color of
        Red ->
            rgb 1 0 0

        Green ->
            rgb 0 1 0

        Yellow ->
            rgb 1 1 0


viewNotDairy : Model -> Element Msg
viewNotDairy model =
    column [ spacing 16 ]
        [ Input.radio
            [ spacing 16 ]
            { onChange = SetFoodStyle
            , selected = Just model.foodStyle
            , label = Input.labelAbove [] (text "Food: ")
            , options =
                [ Input.option (Solid WholeGrain) (text "Solid with Whole Grain")
                , Input.option (Solid NotWholeGrain) (text "Other Solid")
                , Input.option (Liquid Soda) (text "Soda")
                , Input.option (Liquid Alcohol) (text "Alcohol")
                , Input.option (Liquid ArtificialSweetners) (text "Liquid with Artificial Sweetners")
                , Input.option (Liquid RegularLiquid) (text "Other Liquid")
                , Input.option Soup (text "Soup")
                ]
            }
        , let
            caloriesResult : Result (List String) Int
            caloriesResult =
                Validator.run calorieValidator model
          in
          column
            []
            [ Input.text
                []
                { label = Input.labelAbove [] (text "Calories per Serving:")
                , placeholder = Nothing
                , text = model.caloriesPerServing
                , onChange = SetCaloriesPerServing
                }
            , viewErrors caloriesResult
            ]
        , let
            gramResult : Result (List String) Int
            gramResult =
                Validator.run gramsValidator model
          in
          column
            []
            [ Input.text
                []
                { label = Input.labelAbove [] (text "Grams per Serving:")
                , placeholder = Nothing
                , text = model.gramsPerServing
                , onChange = SetGramsPerServing
                }
            , viewErrors gramResult
            ]
        ]


dairySelection : Maybe Dairy -> Element Msg
dairySelection maybeDairy =
    wrappedRow
        [ spacing 16
        , centerX
        , width shrink
        ]
        [ Input.button
            (buttonStyle (maybeDairy == Just NotDairy))
            { label = text "Not Dairy"
            , onPress = Just (SetDairy NotDairy)
            }
        , Input.button
            (buttonStyle (maybeDairy == Just WholeFat))
            { label = text "Dairy Whole Fat"
            , onPress = Just (SetDairy WholeFat)
            }
        , Input.button
            (buttonStyle (maybeDairy == Just LowFat))
            { label = text "Dairy Low Fat"
            , onPress = Just (SetDairy LowFat)
            }
        , Input.button
            (buttonStyle (maybeDairy == Just NonFat))
            { label = text "Dairy Non-Fat"
            , onPress = Just (SetDairy NonFat)
            }
        ]


buttonStyle : Bool -> List (Attribute msg)
buttonStyle selected =
    [ paddingXY 16 8
    , Background.color (rgb 0 1 0.8)
    , Border.solid
    , Border.width 3
    , Border.color <|
        if selected then
            rgb 0 0 1

        else
            rgb 0 1 0.8
    ]


foodColorToString : FoodColor -> String
foodColorToString foodColor =
    case foodColor of
        Green ->
            "Green"

        Yellow ->
            "Yellow"

        Red ->
            "Red"


calculateFoodColor : Model -> Maybe FoodColor
calculateFoodColor model =
    case model.dairy of
        Just NonFat ->
            Just Green

        Just LowFat ->
            Just Yellow

        Just WholeFat ->
            Just Red

        Just NotDairy ->
            case ( Validator.run calorieValidator model, Validator.run gramsValidator model ) of
                ( Ok caloriesPerServing, Ok gramsPerServing ) ->
                    let
                        calorieDensity : Float
                        calorieDensity =
                            toFloat caloriesPerServing / toFloat gramsPerServing
                    in
                    Just <|
                        case model.foodStyle of
                            Solid WholeGrain ->
                                if calorieDensity < 2.4 then
                                    Green

                                else
                                    Yellow

                            Solid NotWholeGrain ->
                                if calorieDensity < 1.0 then
                                    Green

                                else if calorieDensity < 2.4 then
                                    Yellow

                                else
                                    Red

                            Liquid RegularLiquid ->
                                if calorieDensity < 0.4 then
                                    Green

                                else if calorieDensity < 0.5 then
                                    Yellow

                                else
                                    Red

                            Liquid _ ->
                                if calorieDensity < 0.4 then
                                    Yellow

                                else
                                    Red

                            Soup ->
                                if calorieDensity < 0.5 then
                                    Green

                                else if calorieDensity < 1.0 then
                                    Yellow

                                else
                                    Red

                _ ->
                    Nothing

        Nothing ->
            Nothing


viewErrors : Result (List String) value -> Element msg
viewErrors result =
    case result of
        Ok _ ->
            none

        Err errs ->
            column
                []
                (List.map viewError errs)


viewError : String -> Element msg
viewError err =
    paragraph [ Font.color (rgb 1 0 0) ] [ text err ]
