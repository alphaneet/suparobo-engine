# run
    cd processing2D
    sbt run

## fast run
"sbt run" is slowly
    
    scala -cp target/scala-2.9.1/:../models/target/scala-2.9.1/classes:<home_dir>/.sbt/staging/<id>/target/scala-2.9.1/classes:<home_dir>/.ivy2/cache/processing/core_2.9.1/jars/core_2.9.1-1.5.1.jar com.github.alphaneet.suparobo_engine.Main
    

&lt;home_dir&gt; example. my home dir is "Users/alphaneet"
&lt;id&gt; is https://github.com/alphaneet/scala-processing download dir

but it is uncool lol
please other good idea.

# models
    cd models
    sbt test
    
sbt -> ~test is yeah!