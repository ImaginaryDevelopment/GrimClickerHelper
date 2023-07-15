module Components.Strength

open AppDomain.Gen

type StrengthSkill =
    | RageOfDarkness
    | BloodThirstiness
    | FlameFlow
    | Greed
    | March
    | MaskOfFire
    | FireSword
    | BlacksmithCraft
    | BloodyVine
    | Indefatigability
    | ShieldMastery
    | BonfireFlmae
    | ChainStrike
    | Flex
    | General
    | DispelTheDarkness
    | Fireball
    | ApocalypseMask
    | Armageddon
    | FireAura
    with
        static member All = [
            RageOfDarkness
            BloodThirstiness
            FlameFlow
            Greed
            March
            MaskOfFire
            FireSword
            BlacksmithCraft
            BloodyVine
            Indefatigability
            ShieldMastery
            BonfireFlmae
            ChainStrike
            Flex
            General
            DispelTheDarkness
            Fireball
            ApocalypseMask
            Armageddon
            FireAura
        ]
        static member TryParse x =
            match x with
            | null | "" -> None
            | EqualsI "RageOfDarkness" -> RageOfDarkness
            | EqualsI "BloodThirstiness" -> BloodThirstiness
            | EqualsI "FlameFlow" -> FlameFlow
            | EqualsI "Greed" -> Greed
            | EqualsI "March" -> March
            | EqualsI "MaskOfFire" -> MaskOfFire
            | EqualsI "FireSword" -> FireSword
            | EqualsI "BlacksmithCraft" -> BlacksmithCraft
            | EqualsI "BloodyVine" -> BloodyVine
            | EqualsI "Indefatigability"
            | EqualsI "ShieldMastery"
            | EqualsI "BonfireFlmae"
            | EqualsI "ChainStrike"
            | EqualsI "Flex"
            | EqualsI "General"
            | EqualsI "DispelTheDarkness"
            | EqualsI "Fireball"
            | EqualsI "ApocalypseMask"
            | EqualsI "Armageddon"
            | EqualsI "FireAura"

// type Skill = {
//     StatRequired: int
//     Description: string
// }

type Model = {
    // some skills can have only 1 point, you can only have 1 mask
    Skills:Map<StrengthSkill,int> // we'll let the code decide how to handle invalid points
}

type Msg =
    | SkillChange of StrengthSkill * int

let init overrideOpt : Model * Cmd<Msg> =
    {Skills = Map.empty}
