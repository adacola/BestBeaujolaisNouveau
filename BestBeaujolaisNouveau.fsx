(*
フランス・ボージョレーワイン委員会による歴代評価

1995年：ここ数年で一番出来が良い
1996年：10年に1度の逸品
1997年：1976年以来の品質
1998年：10年に1度の当たり年
1999年：品質は昨年より良い
2000年：出来は上々で申し分の無い仕上がり
2001年：ここ10年で最高
2002年：過去10年で最高と言われた01年を上回る出来栄え
2003年：100年に1度の出来
2004年：香りが強く中々の出来栄え
2005年：ここ数年で最高
2006年：昨年同様良い出来栄え
2007年：柔らかく果実味が豊かで上質な味わい
2008年：豊かな果実味と程よい酸味が調和した味
2009年：50年に1度の出来
2010年：1950年以降最高の出来といわれた2009年と同等の出来
2011年：近年の当たり年である2009年に匹敵する出来
2012年：史上最悪の不作だが、ブドウの品質はよく熟すことができて健全

問題：上記の情報から最も出来が良い年を求めよ。
*)

type Years = Years

type Evaluation = Good | Bad | Equal | Vague

/// フランス・ボージョレーワイン委員会による歴代評価から、最も出来が良い年のリストと、判別不能な年のリストを求める。
/// 戻り値 : (最も出来が良い年のリスト, 判別不能な年のリスト)
let bestBeaujolaisNouveau() =

    /// 「ここ数年」が表す具体的な年数
    let few = 3
    /// ここn年で最も出来が良い
    let isTheBestForThePast n (_ : Years) thisYear = function x when thisYear - n = x -> Equal | x when thisYear - n < x -> Good | _ -> Vague
    /// year年以来の出来
    let isTheBestSince year _ = function x when x = year -> Equal | x when year < x -> Good | _ -> Vague
    /// 去年より出来が良い
    let isBetterThanLastYear thisYear = function x when x = thisYear - 1 -> Good | _ -> Vague
    /// 去年と同等の出来
    let isEqualToLastYear thisYear = function x when x = thisYear - 1 -> Equal | _ -> Vague
    /// 素晴らしい出来
    let isVeryGood _ _ = Vague
    /// 良い出来
    let isGood = isVeryGood

    /// フランス・ボージョレーワイン委員会による歴代評価
    let beaujolaisNouveaus =
        [
            1995, isTheBestForThePast few Years
            1996, isTheBestForThePast 10 Years
            1997, isTheBestSince 1976
            1998, isTheBestForThePast 10 Years
            1999, isBetterThanLastYear
            2000, isVeryGood
            2001, isTheBestForThePast 10 Years
            2002, isBetterThanLastYear
            2003, isTheBestForThePast 100 Years
            2004, isVeryGood
            2005, isTheBestForThePast few Years
            2006, isEqualToLastYear
            2007, isVeryGood
            2008, isVeryGood
            2009, isTheBestForThePast 50 Years
            2010, isEqualToLastYear
            2011, isEqualToLastYear
            2012, isGood
        ] |> Seq.map (fun (year, compare) -> year, function x when x = year -> Equal | x when year < x -> Vague | x -> compare year x)
        |> Map.ofSeq

    // 最も出来が良い年のリストと、判別不能な年のリストを求める
    let startYear::restYears = beaujolaisNouveaus |> Map.toSeq |> Seq.map fst |> Seq.toList
    let maxYears, vagueYears =
        (([startYear], []), restYears) ||> List.fold (fun ((maxYear::_ as maxYears), vagueYears) year ->
            match beaujolaisNouveaus.[year] maxYear with
            | Good -> [year], []
            | Bad -> maxYears, vagueYears
            | Equal -> year::maxYears, vagueYears
            | Vague -> maxYears, year::vagueYears)
    List.sort maxYears, List.sort vagueYears

let maxYears, vagusYears = bestBeaujolaisNouveau()
printfn "最も出来が良い年は %A 、判別不能な年は %A" maxYears vagusYears
