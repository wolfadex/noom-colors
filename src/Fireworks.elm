module Fireworks exposing (..)

import Browser.Events
import Element exposing (..)
import Html.Attributes
import Particle exposing (Particle)
import Particle.System as System exposing (System)
import Random exposing (Generator, initialSeed)
import Random.Extra
import Random.Float
import Svg exposing (Svg)
import Svg.Attributes as SAttrs


type alias Model =
    { particles : System Firework
    , windowSize : { width : Int, height : Int }
    }


type Firework
    = Fizzler FColor


type FColor
    = FRed
    | FGreen
    | FBlue


init : { initialSeed : Random.Seed, windowWidth : Int, windowHeight : Int } -> Model
init { initialSeed, windowWidth, windowHeight } =
    { particles = System.init initialSeed
    , windowSize = { width = windowWidth, height = windowHeight }
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ System.sub [] ParticleMsg model.particles
        , Browser.Events.onResize WindowResize
        ]


type Msg
    = ParticleMsg (System.Msg Firework)
    | WindowResize Int Int


update : Msg -> Model -> Model
update msg model =
    case msg of
        ParticleMsg inner ->
            { model | particles = System.update inner model.particles }

        WindowResize w h ->
            { model | windowSize = { width = w, height = h } }


burst : Model -> Model
burst model =
    { model
        | particles =
            System.burst
                (Random.Extra.andThen3 fireworkAt
                    (Random.uniform FRed [ FGreen, FBlue ])
                    (Random.Float.normal (toFloat model.windowSize.width / 2) 100)
                    (Random.Float.normal (toFloat model.windowSize.height / 4) 100)
                )
                model.particles
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
        |> Particle.withSpeed (Random.map (clamp 0 200) (Random.Float.normal 100 100))
        |> Particle.withLifetime (Random.Float.normal 1.25 0.1)


view : Model -> Element msg
view model =
    System.view fireworkView
        [ Html.Attributes.style "width" (String.fromInt model.windowSize.width ++ "px")
        , Html.Attributes.style "height" (String.fromInt model.windowSize.height ++ "px")
        , Html.Attributes.style "background-color" "#BFBFBF"
        ]
        model.particles
        |> html


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
