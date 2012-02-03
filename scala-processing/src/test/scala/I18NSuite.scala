package com.github.alphaneet.scala_processing

class I18NSuite extends FunSuite with ShouldMatchers {
  trait Japanese {
    val ja = <ja>
    <test>てすと</test>
    
    <scene>
      <title>ハロワ</title>
      <actions>
        <index>一覧</index>
        <edit>編集</edit>
      </actions>
    </scene>
    </ja>

    val i18n = new I18N("ja", ja)
    def t(name: String) = i18n.t(name)
  }

  trait English {
    val en = <en>
    <test>TEST</test>
    
    <scene>
      <title>hello work</title>
      <actions>
        <index>INDEX</index>
        <edit>EDIT</edit>
      </actions>    
    </scene>    
    </en>

    val i18n = new I18N("en", en)
    def t(name: String) = i18n.t(name)    
  }

  // ja: rails の i18n#t(ranslate) のように使う
  test("#t(ranslate) uses like rails's i18n#t()") {
    new Japanese { t("test") should be ("てすと") }
    new English  { t("test") should be ("TEST") }
  }

  // ja: '.' で単語をくくるとネストする
  test("it nests, if '.' is used.") {
    new Japanese {
      t("scene.title") should be ("ハロワ")
      t("scene.actions.index") should be ("一覧")
      t("scene.actions.edit")  should be ("編集")
    }

    new English {
      t("scene.title") should be ("hello work")
      t("scene.actions.index") should be ("INDEX")
      t("scene.actions.edit")  should be ("EDIT")      
    }
  }  
  
  // ja: #t(ranslate) は存在しない名前を指定すると . が スペースに置換された名前が返ってくる
  test("if #t specifies the nothing name, it return '.' was replaced space") {
    new Japanese { t("oppai.pero.pero") should be ("oppai pero pero") }
    new English  { t("oppai.pero.pero") should be ("oppai pero pero") }
  }

  // ja: #translateOrException は存在しない名前を指定すると例外") {
  test("if #translateOrException specifies the nothing name, throw Exception") {
    new Japanese {
      evaluating {
        i18n.translateOrException("oppai.pero.pero")
      } should produce [NoSuchElementException]
    }
    
    new English  {
      evaluating {
        i18n.translateOrException("oppai.pero.pero")
      } should produce [NoSuchElementException]
    }
  }
}
