module Main exposing (Dairy, Flags, FoodStyle, Model, Msg, main)

import Browser exposing (Document)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Fireworks
import Html
import Html.Attributes
import Html.Events
import Html.Styled
import Random
import Select exposing (Action(..))
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
    , foodStyleState : Select.State
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
      , foodStyleState = Select.initState
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
    | FireworksMessage Fireworks.Msg
    | FoodStyleSelectMessage (Select.Msg FoodStyle)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map FireworksMessage (Fireworks.subscriptions model.fireworks)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    (case msg of
        SetCaloriesPerServing caloriesPerServing ->
            Update.Pipeline.save { model | caloriesPerServing = caloriesPerServing }

        SetGramsPerServing gramsPerServing ->
            Update.Pipeline.save { model | gramsPerServing = gramsPerServing }

        SetDairy dairyFat ->
            Update.Pipeline.save { model | dairy = Just dairyFat }

        FoodStyleSelectMessage message ->
            let
                ( maybeAction, updatedState, cmds ) =
                    Select.update message model.foodStyleState
            in
            ( { model
                | foodStyleState = updatedState
                , foodStyle =
                    case maybeAction of
                        Just (Select style) ->
                            style

                        _ ->
                            model.foodStyle
              }
            , Cmd.map FoodStyleSelectMessage cmds
            )

        FireworksMessage message ->
            Update.Pipeline.save { model | fireworks = Fireworks.update message model.fireworks }
    )
        |> Tuple.mapFirst triggerFireworks


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
                model.fireworks
                    |> Fireworks.burst
                    |> Fireworks.burst
                    |> Fireworks.burst
                    |> Fireworks.burst
                    |> Fireworks.burst

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


selectedFoodStyleToMenuItem : FoodStyle -> Select.MenuItem FoodStyle
selectedFoodStyleToMenuItem foodStyle =
    case foodStyle of
        Solid WholeGrain ->
            { item = Solid WholeGrain, label = "Solid with Whole Grain" }

        Solid NotWholeGrain ->
            { item = Solid NotWholeGrain, label = "Other Solid" }

        Liquid Soda ->
            { item = Liquid Soda, label = "Soda" }

        Liquid Alcohol ->
            { item = Liquid Alcohol, label = "Alcohol" }

        Liquid ArtificialSweetners ->
            { item = Liquid ArtificialSweetners, label = "Liquid with Artificial Sweetners" }

        Liquid RegularLiquid ->
            { item = Liquid RegularLiquid, label = "Other Liquid" }

        Soup ->
            { item = Soup, label = "Soup" }


foodStyleOptions : List FoodStyle
foodStyleOptions =
    [ Solid WholeGrain
    , Solid NotWholeGrain
    , Liquid Soda
    , Liquid Alcohol
    , Liquid ArtificialSweetners
    , Liquid RegularLiquid
    , Soup
    ]


viewNotDairy : Model -> Element Msg
viewNotDairy model =
    column [ spacing 16, width fill ]
        [ text "Food:"
        , Select.view
            (selectedFoodStyleToMenuItem model.foodStyle
                |> Just
                |> Select.single
                |> Select.state model.foodStyleState
                |> Select.menuItems (List.map selectedFoodStyleToMenuItem foodStyleOptions)
            )
            (Select.selectIdentifier "Food:")
            |> Html.Styled.toUnstyled
            |> html
            |> map FoodStyleSelectMessage
            |> el [ width fill ]
        , let
            caloriesResult : Result (List String) Int
            caloriesResult =
                Validator.run calorieValidator model
          in
          column
            []
            [ numberInput
                { label = "Calories per Serving:"
                , value = model.caloriesPerServing
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
            [ numberInput
                { label = "Grams per Serving:"
                , value = model.gramsPerServing
                , onChange = SetGramsPerServing
                }
            , viewErrors gramResult
            ]
        ]


numberInput : { label : String, value : String, onChange : String -> msg } -> Element msg
numberInput config =
    column
        [ spacing 4 ]
        [ text config.label
        , Html.input
            [ Html.Attributes.type_ "number"
            , Html.Events.onInput config.onChange
            , Html.Attributes.min "0"
            , Html.Attributes.value config.value
            , Html.Attributes.style "font-size" "20px"
            , Html.Attributes.style "padding" "8px"
            ]
            []
            |> html
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
