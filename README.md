# run
    cd processing2D
    sbt run

## fast run
"sbt run" is slowly
    
    scala -cp target/scala-2.9.1/:../models/target/scala-2.9.1/classes:<home_dir>/.sbt/staging/<id>/target/scala-2.9.1/classes:<home_dir>/.ivy2/cache/processing/core_2.9.1/jars/core_2.9.1-1.5.1.jar com.github.alphaneet.suparobo_engine.Main
    

&lt;home_dir&gt; your home dir. example is "Users/alphaneet".

&lt;id&gt; is https://github.com/alphaneet/scala-processing download dir.


### TODO: want to use sbt plugin sometime.
https://github.com/sbt/sbt-assembly
https://github.com/typesafehub/xsbt-start-script-plugin


# game logic testing
    cd models
    sbt test
   
# tools
    cd tools
    sbt run
    
    Multiple main classes detected, select one to run:

    [1] com.github.alphaneet.suparobo.MapEditor
    [2] com.github.alphaneet.suparobo.DiagramEditor
    [3] com.github.alphaneet.suparobo.CharactersEditor
    [4] com.github.alphaneet.suparobo.UIEditor