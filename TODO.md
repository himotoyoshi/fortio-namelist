
TODO
----

### `FortIO::Namelist.edit` の実装構想

`require "fortio-namelist/edit"` で有効化される、体裁を保持したnamelist編集機能。
基盤として Textura ライブラリを利用する。

#### APIイメージ

```ruby
require "fortio-namelist/edit"

FortIO::Namelist.edit("config.nml") do |nml|
  nml.group(:nampar) do |g|
    g[:dt] = 12         # 既存変数の値を差し替え
    g[:newvar] = 100     # 新規変数の追加
  end
end
# コメント・インデント・空行はそのまま、値だけ変わる
```

#### 内部レイヤー構造

| edit の API層 | Textura層 | 役割 |
|---|---|---|
| `nml` | `Document` | ファイル全体、原子的書き込み |
| `nml.group(:name)` のブロック | `Slice` (`scan` の行範囲で切り出し) | グループ区間の体裁保持 |
| `g[:var] = val` | Form的な独自クラス | 名前付きキャプチャで値部分だけ差し替え |

#### 処理フロー

1. **scan** — 各グループの行範囲と変数一覧を特定
2. **Textura::Document** — ファイルを開く
3. **Slice** — `scan` の `lines` で対象グループ区間を切り出し
4. **Form的クラス** — 既存変数は名前付きキャプチャで値を差し替え、新規変数は周囲のスタイルに合わせて行を挿入
5. **parse** — 編集後のテキスト全体をパースして構文検証（壊れたnamelistは書き戻さない）
6. **atomic write** — Textura の原子的書き込みで安全にファイルに反映

#### 設計ポイント

- `textura` はオプショナル依存。`require "fortio-namelist/edit"` しなければ textura なしで従来通り使える
- 新規変数の挿入時は、周囲の行のインデント・アラインメント・セパレータに合わせてフォーマット。fortio-namelist の dump のフォーマットオプションを流用可能
- 書き戻し前に `FortIO::Namelist.parse` で結果を検証するガードレール。ユーザが意識しなくても壊れたファイルは生成されない
- f90nml の `patch()` に相当する機能を、汎用テキスト編集フレームワーク(Textura)の上に実現する

#### 前提

- Textura の公開・安定化
- `FortIO::Namelist.scan` の実装（済）
