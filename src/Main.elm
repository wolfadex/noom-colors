module Main exposing (Dairy, Flags, FoodStyle, Model, Msg, main)

import Browser exposing (Document)
import Element exposing (..)
import Element.Background as Background
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
    , isWholeGrain : Bool
    , foodStyle : FoodStyle
    , dairy : Maybe Dairy
    , previousFoodColorIsGreen : Bool
    , fireworks : Fireworks.Model
    }


type FoodStyle
    = Solid
    | Liquid LiquidType
    | Soup


type LiquidType
    = ArtificialSweetners
    | Soda
    | Alcohol
    | RegularLiquid


type Dairy
    = NonFat
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
      , isWholeGrain = False
      , foodStyle = Solid
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
    | SetWholeGrain Bool
    | SetDairy Dairy
    | ClearDairy
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

        SetWholeGrain isWholeGrain ->
            { model | isWholeGrain = isWholeGrain }

        SetDairy dairyFat ->
            { model | dairy = Just dairyFat }

        ClearDairy ->
            { model | dairy = Nothing }

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
        [ row
            [ spacing 16 ]
            [ Input.radio
                [ spacing 8 ]
                { onChange = SetDairy
                , selected = model.dairy
                , label = Input.labelLeft [] (text "Dairy Fat?: ")
                , options =
                    [ Input.option NonFat (text "Non-Fat")
                    , Input.option LowFat (text "Low Fat")
                    , Input.option WholeFat (text "Whole Fat")
                    ]
                }
            , Input.button
                [ paddingXY 16 8
                , Background.color (rgb 0 1 0.8)
                , centerY
                ]
                { label = text "Not Dairy"
                , onPress = Just ClearDairy
                }
            ]
        , case model.dairy of
            Just _ ->
                none

            Nothing ->
                Input.radio
                    [ spacing 16 ]
                    { onChange = SetFoodStyle
                    , selected = Just model.foodStyle
                    , label = Input.labelAbove [] (text "Food Style")
                    , options =
                        [ Input.option Solid (text "Solid")
                        , Input.option (Liquid RegularLiquid) (text "Liquid - Regular")
                        , Input.option (Liquid Soda) (text "Liquid - Soda")
                        , Input.option (Liquid ArtificialSweetners) (text "Liquid - Artificial Sweetners")
                        , Input.option (Liquid Alcohol) (text "Liquid - Alcohol")
                        , Input.option Soup (text "Soup")
                        ]
                    }
        , case model.dairy of
            Just _ ->
                none

            Nothing ->
                let
                    caloriesResult : Result (List String) Int
                    caloriesResult =
                        Validator.run calorieValidator model
                in
                column
                    []
                    [ Input.text
                        []
                        { label = Input.labelAbove [] (text "Calories per Serving")
                        , placeholder = Nothing
                        , text = model.caloriesPerServing
                        , onChange = SetCaloriesPerServing
                        }
                    , viewErrors caloriesResult
                    ]
        , case model.dairy of
            Just _ ->
                none

            Nothing ->
                let
                    gramResult : Result (List String) Int
                    gramResult =
                        Validator.run gramsValidator model
                in
                column
                    []
                    [ Input.text
                        []
                        { label = Input.labelAbove [] (text "Grams per Serving")
                        , placeholder = Nothing
                        , text = model.gramsPerServing
                        , onChange = SetGramsPerServing
                        }
                    , viewErrors gramResult
                    ]
        , case model.dairy of
            Just _ ->
                none

            Nothing ->
                case model.foodStyle of
                    Solid ->
                        Input.checkbox
                            []
                            { label = Input.labelAbove [] (text "Is Whole Grain?")
                            , icon = Input.defaultCheckbox
                            , checked = model.isWholeGrain
                            , onChange = SetWholeGrain
                            }

                    _ ->
                        none
        , paragraph []
            [ text "Your food is: "
            , case calculateFoodColor model of
                Nothing ->
                    text "Unknown"

                Just foodColor ->
                    foodColorToString foodColor
                        |> text
                        |> el
                            [ Font.color <|
                                case foodColor of
                                    Green ->
                                        rgb 0 1 0

                                    Yellow ->
                                        rgb 1 1 0

                                    Red ->
                                        rgb 1 0 0
                            ]
            ]
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

        Nothing ->
            case ( Validator.run calorieValidator model, Validator.run gramsValidator model ) of
                ( Ok caloriesPerServing, Ok gramsPerServing ) ->
                    let
                        calorieDensity : Float
                        calorieDensity =
                            toFloat caloriesPerServing / toFloat gramsPerServing
                    in
                    Just <|
                        case model.foodStyle of
                            Solid ->
                                if model.isWholeGrain then
                                    if calorieDensity < 2.4 then
                                        Green

                                    else
                                        Yellow

                                else if calorieDensity < 1.0 then
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
