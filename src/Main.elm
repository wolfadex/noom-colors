module Main exposing (main)

import Browser exposing (Document)
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Html.Attributes
import Particle exposing (Particle)
import Particle.System as System exposing (System)
import Random exposing (Generator)
import Random.Extra
import Random.Float exposing (normal)
import Svg exposing (Svg)
import Svg.Attributes as SAttrs
import Update.Pipeline
import Validator exposing (Validator)


main : Program Int Model Msg
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
    , fireworks : System Firework
    , previousFoodColorIsGreen : Bool
    }


type Firework
    = Fizzler FColor


type FColor
    = FRed
    | FGreen
    | FBlue


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


init : Int -> ( Model, Cmd Msg )
init initialSeed =
    ( { caloriesPerServing = "0"
      , gramsPerServing = "0"
      , isWholeGrain = False
      , foodStyle = Solid
      , dairy = Nothing
      , fireworks = System.init (Random.initialSeed initialSeed)
      , previousFoodColorIsGreen = False
      }
    , Cmd.none
    )


type Msg
    = NoOp
    | SetCaloriesPerServing String
    | SetGramsPerServing String
    | SetWholeGrain Bool
    | SetDairy Dairy
    | ClearDairy
    | SetFoodStyle FoodStyle
    | ParticleMsg (System.Msg Firework)


subscriptions : Model -> Sub Msg
subscriptions model =
    System.sub [] ParticleMsg model.fireworks


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    (case msg of
        NoOp ->
            model

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

        ParticleMsg inner ->
            { model | fireworks = System.update inner model.fireworks }
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
                System.burst
                    (Random.Extra.andThen3 fireworkAt
                        (Random.uniform FRed [ FGreen, FBlue ])
                        (normal 300 100)
                        (normal 300 100)
                    )
                    model.fireworks

            else
                model.fireworks
    }


fireworkAt : FColor -> Float -> Float -> Generator (List (Particle Firework))
fireworkAt color x y =
    fizzler color
        |> Particle.withLocation (Random.constant { x = x, y = y })
        |> Particle.withGravity 50
        |> Particle.withDrag
            (\_ ->
                { coefficient = 1
                , density = 0.015
                , area = 2
                }
            )
        |> Random.list 150


fizzler : FColor -> Generator (Particle Firework)
fizzler color =
    Particle.init (Random.constant (Fizzler color))
        |> Particle.withDirection (Random.map degrees (Random.float 0 360))
        |> Particle.withSpeed (Random.map (clamp 0 200) (normal 100 100))
        |> Particle.withLifetime (normal 1.25 0.1)


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
        [ layout [ width fill, height fill ] (viewModel model) ]
    }


viewModel : Model -> Element Msg
viewModel model =
    let
        caloriesResult : Result (List String) Int
        caloriesResult =
            Validator.run calorieValidator model

        gramResult : Result (List String) Int
        gramResult =
            Validator.run gramsValidator model
    in
    column
        [ padding 16
        , spacing 16
        , centerX
        ]
        [ html <|
            System.view fireworkView
                [ Html.Attributes.style "width" "600px"
                , Html.Attributes.style "height" "600px"
                , Html.Attributes.style "background-color" "#0F0F0F"
                ]
                model.fireworks
        , row
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


fireworkView : Particle Firework -> Svg msg
fireworkView particle =
    case Particle.data particle of
        Fizzler color ->
            let
                length =
                    max 2 (Particle.speed particle / 15)

                ( hue, saturation, luminance ) =
                    toHsl color

                maxLuminance =
                    100

                luminanceDelta =
                    maxLuminance - luminance

                lifetime =
                    Particle.lifetimePercent particle

                opacity =
                    if lifetime < 0.1 then
                        lifetime * 10

                    else
                        1
            in
            Svg.ellipse
                [ -- location within the burst
                  SAttrs.cx (String.fromFloat (length / 2))
                , SAttrs.cy "0"

                -- size, smeared by motion
                , SAttrs.rx (String.fromFloat length)
                , SAttrs.ry "2"
                , SAttrs.transform ("rotate(" ++ String.fromFloat (Particle.directionDegrees particle) ++ ")")

                -- color!
                , SAttrs.opacity (String.fromFloat opacity)
                , SAttrs.fill
                    (hslString
                        hue
                        saturation
                        (maxLuminance - luminanceDelta * (1 - lifetime))
                    )
                ]
                []


{-| Using the tango palette, but a little lighter. Original colors at
-}
toHsl : FColor -> ( Float, Float, Float )
toHsl color =
    case color of
        FRed ->
            -- scarlet red
            ( 0, 86, 75 )

        FGreen ->
            -- chameleon
            ( 90, 75, 75 )

        FBlue ->
            -- sky blue
            ( 211, 49, 83 )


hslString : Float -> Float -> Float -> String
hslString hue saturation luminance =
    "hsl("
        ++ String.fromFloat hue
        ++ ","
        ++ String.fromFloat saturation
        ++ "%,"
        ++ String.fromFloat luminance
        ++ "%)"


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
