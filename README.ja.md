# R2camtrapdp

<!-- badges: start -->
<!-- [![CRAN status](https://www.r-pkg.org/badges/version/R2camtrapdp)](https://CRAN.R-project.org/package=R2camtrapdp) -->
<!-- [![R-CMD-check](https://github.com/kfukasawa37/R2camtrapdp/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/kfukasawa37/R2camtrapdp/actions/workflows/R-CMD-check.yaml) -->
<!-- badges: end -->

*（English README: [README.md](README.md)）*

任意のスプレッドシート形式のカメラトラップ（および音響）データを、カメラトラップ
データの Frictionless データパッケージ標準である
[Camera Trap Data Package (Camtrap DP)](https://camtrap-dp.tdwg.org/) に変換します。

`R2camtrapdp` は**スキーマ駆動**です。出力テーブルの構造・型・制約・リレーションは、
指定した Camtrap DP バージョンの公式 Frictionless *table schema* から読み取られます。
そのため、任意の Camtrap DP バージョン（`1.0` / `1.0.1` / `1.0.2`）に加え、
[bioacoustics 拡張](https://github.com/camera-traps/bioacoustics)のような別フレーバー
にも、独自列・追加列を含めて対応します（ルールをハードコードしません）。

## できること

1. **殻の導出** — 指定バージョンのスキーマから、空テーブルと制約を導出。
2. **構築と検証** — 列をマッピングし、型変換し、スキーマ制約（`required`,
   `unique`, `enum`, `minimum`/`maximum`, `pattern`, 日付・日時の **format** ほか）
   とテーブル間リレーション（主キー・外部キー）に照らして検証。
3. **組み立て** — データパッケージ（`deployments.csv`, `media.csv`,
   `observations.csv`, `datapackage.json`）を生成。
4. **検証** — Python [Frictionless](https://framework.frictionlessdata.io/) で
   書き出したパッケージを検証し、エラーを R 側で整形（ファイル・列・行・規則・
   **問題の実値**）。

## インストール

```r
# GitHub から（build_vignettes = TRUE で vignette も併せて導入）
# install.packages("devtools")
devtools::install_github("kfukasawa37/R2camtrapdp", build_vignettes = TRUE)

# （CRAN 登録後）
# install.packages("R2camtrapdp")
```

Python の Frictionless バリデータ（ステップ4）は任意で、
`validate_frictionless()` / `ctdp_validate_frictionless()` を使う場合のみ必要です。

```sh
pip install frictionless
```

## クイックスタート

```r
library(R2camtrapdp)

data("Idep")   # デプロイメント表（例）
data("Iobs")   # 観察表（例）

# 1-2. 3つの中核テーブルを作成（カメラトラップ用ヘルパ）
deployments <- create_deployments(
  deploymentID = Idep$deploymentID, latitude = Idep$latitude, longitude = Idep$longitude,
  deploymentStart_date = Idep$startDate, deploymentStart_time = Idep$startTime,
  deploymentEnd_date = Idep$endDate, deploymentEnd_time = Idep$endTime,
  cameraID = Idep$cameraID, setupBy = Idep$setupBy)

# 3. データパッケージを組み立て（set_*() でスキーマ駆動の検証が走る）
dp <- R6_CamtrapDP$new(version = "1.0.1", title = "My dataset", description = "...")
dp$set_deployments(deployments)
# dp$set_media(media); dp$set_observations(observations)
dp$add_contributors(data.frame(title = "Jane Doe", role = "contact"))
dp$add_license(name = "CC0-1.0", scope = "data")
dp$set_project(title = "Project X", samplingDesign = "opportunistic",
               captureMethod = "activityDetection", individualAnimals = FALSE,
               observationLevel = "media")
dp$set_st()
dp$check_relations()                                  # 主キー・外部キー

path <- file.path(tempdir(), "my-camtrapdp")
dp$out_camtrapdp(write = TRUE, directory = path)      # CSV + datapackage.json

# 4. Python Frictionless で検証
issues <- dp$validate_frictionless(directory = path, python = "python")
ctdp_is_valid(issues)
```

実行可能な一連のスクリプト（スキーマ確認 → マッピング → 構築 →
リレーション/メタデータ検査 → 書き出し → Frictionless 検証）は
[`examples/example_usage.R`](examples/example_usage.R) にあります:

```r
source("r2camtrapdp.R")      # または: library(R2camtrapdp)
# その後 examples/example_usage.R を実行
```

任意スプレッドシートのマッピング、音声（音響）データのワークフロー、検証の詳細は
vignette を参照してください。

## 主な関数・クラス

### テーブルビルダ

| 関数 | 用途 |
|---|---|
| `create_deployments()`, `create_media()`, `create_observations()` | 3つの中核テーブルを作成（カメラトラップ用の便利ヘルパ）。 |
| `ctdp_build_table(schema, data, mapping, datetime_merges)` | 任意バージョン/フレーバー・独自列対応の汎用スキーマ駆動ビルダ。 |
| `ctdp_apply_mapping(df, mapping)` | 元列名を Camtrap DP フィールド名へ改名。 |
| `ctdp_merge_datetime(df, date_col, time_col, target)` | 別々の日付・時刻列を datetime に結合。 |

### スキーマ／メタデータ profile の確認（R6）

* **`TableSchema`** — Frictionless table schema:
  `field_names()`, `required_field_names()`, `requirements()`, `empty_table()`,
  `coerce()`, `validate()`, `check_schema()`, `external_references()`,
  `semantic_only_fields()`。
* **`MetadataProfile`** — パッケージ profile（必須メタデータ構造）。

### データパッケージ・ビルダ（R6）: `R6_CamtrapDP`

| メソッド | 用途 |
|---|---|
| `set_deployments()` / `set_media()` / `set_observations()` | 中核テーブルを追加。スキーマ型へ変換し検証。 |
| `add_table()`, `set_custom()` | 独自／追加リソースを追加。 |
| `add_contributors()`, `add_sources()`, `add_license()`, `set_project()`, `set_st()`, `set_taxon()`, `add_relatedIdentifiers()`, `add_references()`, `set_properties()`, `update_created()` | メタデータ（`update_created()` は `created` 日時を設定）。 |
| `get_schema()`, `get_profile()`, `import_metadata()` | table schema／パッケージ profile の取得・キャッシュ、リストからのメタデータ取込。 |
| `metadata_requirements()`, `check_metadata()` | profile 由来の必須メタデータの一覧／検査。 |
| `check_relations()` | テーブル間の主キー・外部キー整合性。 |
| `check_descriptor()`, `check_camtrap_profile()`, `external_references()` | 適合事前チェック／URL 参照の洗い出し。 |
| `validate(relations, metadata, conformance, frictionless)` | 集約検証。 |
| `out_camtrapdp(write, directory)` | データパッケージの返却／書き出し。 |
| `validate_frictionless(directory)` | 書き出したパッケージを Python Frictionless で検証。 |

### 検証ヘルパ

| 関数 | 用途 |
|---|---|
| `ctdp_validate_frictionless(directory)` | 既存パッケージを**上書きせず**検証。 |
| `ctdp_check_schema(x)` | table schema の well-formedness（型・制約・キー）を検査。 |
| `ctdp_schema_references(x)`, `ctdp_semantic_only_fields(x)` | スキーマの URL 参照／参照のみで定義される列を一覧。 |
| `ctdp_parse_frictionless(report)`, `ctdp_summarize_validation(issues)`, `ctdp_is_valid(issues)`, `ctdp_issues()` | 統一 issue テーブルの構築・要約・判定。 |

各検査は、`source`・`field`・`row`・`constraint`・`value`（問題の実値）・
`severity`・`message`・`engine` の列を持つ整形済み **issue テーブル**を返します。

## バージョンとフレーバー

* Camtrap DP **1.0 / 1.0.1 / 1.0.2** に対応。（公式の `1.0` *profile* には新しい
  Frictionless が拒否する上流バグがあり、`validate_frictionless()` が自動回避します
  が、`1.0.1` 以降を推奨。）
* **音響（bioacoustics）**フレーバーは `set_properties(schema_urls=, profile=)` で
  スキーマ／profile を指定して対応。音声データはメディアベースです（音響 vignette 参照）。

> **ヒント:** 日時は `POSIXct` で渡してください。各テーブルが要求する日時書式
> （UTC オフセット、必要箇所には小数秒）で書き出されます。文字列の日時はそのまま
> 書き出されます。

## 同梱データ

* `Vdep`, `Vobs` — 単一カメラトラップの例。`Vdep` は1台のカメラトラップ（NIES, Japan）
  の設置データ、`Vobs` はその**動画**データ。
* `Idep`, `Iobs` — 複数カメラトラップの例（ダミー）。`Idep` は10件の設置、`Iobs` は
  その**画像**データ。
* `Adep`, `Aobs` — 音響例（設置野帳／ファイル名つき観察野帳。`media` はファイル名から生成）。
* `datapackageVdata`, `datapackageIdata` — 事前構築済みの Camtrap DP オブジェクト
  （それぞれ `Vdep`/`Vobs`、`Idep`/`Iobs` から作成）。

## Vignette

`vignette(...)` で開けます:

```r
library(R2camtrapdp)
vignette("Vignette_R2camtrapdp")               # 複数カメラトラップ
vignette("Vignette_R2camtrapdp_SingleCamera")  # 単一カメラトラップ
vignette("Vignette_R2camtrapdp_SchemaDriven")  # スキーマ駆動ワークフロー（_ja もあり）
vignette("Vignette_R2camtrapdp_Audio")         # 音声（音響）データ（_ja もあり）
```

## 依存関係

* **R**: R6, jsonlite, tibble, magrittr, lubridate, dplyr, tidyr, purrr, readr,
  httr, taxadb（`set_taxon()` 用）。
* **Suggests**: camtrapdp（出力オブジェクトはクラス `camtrapdp` を持ち、camtrapdp
  リーダーパッケージとの相互運用のため）, knitr, rmarkdown, testthat, jsonvalidate。
* **システム（任意）**: `validate_frictionless()` 用に Python ＋ `frictionless`。

## ライセンス

未定（例: MIT）。`LICENSE` を参照。

---

開発者向け: `MANUAL.md`（アーキテクチャ・内部仕様）と `ROADMAP.md`
（正式パッケージ化／CRAN ロードマップ）を参照。
