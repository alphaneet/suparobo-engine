package com.github.alphaneet.suparobo

// マップの外に出た場合
class OutsideBoardException(msg: String = "") extends Exception(msg)

// 範囲の外に出た場合
class OutsideRangeException(msg: String = "") extends Exception(msg)

// その座標に既に別の Character がいたり、
// 移動不可能な地形の座標に Character を配置した場合
class UsedBoardPositionException(msg: String = "") extends Exception(msg)

// 対象のキャラクターが見つからないアクションを起こした場合
// （例：キャラクターがいない座標に攻撃する）
class NotFoundCharacterException(msg: String = "") extends Exception(msg)

// チャンピオンをセットしてない場合
class NoSuchChampionException(msg: String = "") extends Exception(msg)

// コストオーバーの場合
class OverCostException(msg: String = "") extends Exception(msg)
