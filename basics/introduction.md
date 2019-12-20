## why Erlang

1. The application needs to handle a large number of concurrent activities
    * https://brownbears.tistory.com/213
    * concurrent란 : Parallel computing과는 다르다!
    * concurrent : 1-core n-process로 process들을 switch하면서 마치 동시에 진행되는 것처럼 처리하는 기술
        개인적으로 바로 떠오르는 것은 multi-threading, multi-processing
    * parallel : n-core n(or m)process로 실제로 동시에 실행되는 것, 
        GPU를 사용한 그래픽처리, 딥러닝학습이 떠오르네
2. It should be easily distributable over a network of computers.

3. There should be a facility to make the app. fault-tolerant to both software and hardware error 

4. The app. should be scalable. This means that it should have the ability to span across multiple servers with little or no change.

5. It should be easily upgradable and reconfigurable without having to stop and restart the app. itself.

6. The application should be responsive to users within certain strict timeframes.
