## Chapter 9. 랭킹과 알고리즘 

### 랭킹

* 어떤 규칙에 의해서 리스트의 값들에 순위를 매기는 것을 의미한다.
* 한국 기원에서 사용하는 랭킹 규칙은 물리학 교수, 체스 마스터인 Arpad Elo가 만든 [Elo rating system](https://en.wikipedia.org/wiki/Elo_rating_system)에 기초를 두고 있다. 

* 구글은 [페이지 랭크](https://en.wikipedia.org/wiki/PageRank)라는 알고리즘을 만들어서 사용하고 있다! 

* 랭킹 기능은 무엇을 기준으로 순위를 산정해야 하는가에 달려 있다. 

### 정렬 알고리즘 

* 자신이 개발하는 솔루션이 큰 문제에 부딪혔을 때 그 문제를 해결할 수 있는 사람은 기본 원리를 충실히 연구해서 실제로 코드가 어떻게 동작하는 지 확실히 알고 있는 사람!! 그런 사람이 되자

* lists, ordsets, dict, gb_trees, ets를 알아볼 것이다.

### lists 

* singly linked list 데이터 구조와 흡사하네

* lists:sort(Src) -> Dst, 얼랭에서 리스트를 정렬하느데 사용할 수 있는 함수
    
    * Merge sort를 사용했다. 성능을 최적화 시킨.
    * 왜 Merge sort를 사용했지?

        |algo|Best|Avg.|Worst|Memory|Stable|
        |---|---|---|---|:---:|:---:|
        |Insertion|n|n^2|n^2|1|o|
        |Selection|n^2|n^2|n^2|1|x|
        |Bubble|n|n^2|n^2|1|o|
        |Shell|n|n(logn)^2|n(logn)^2|1|x|
        |Merge|nlogn|nlogn|nlogn|n|o|
        |Heap|nlogn|nlogn|nlogn|1|x|
        |Qucik|nlogn|nlogn|n^2|logn or n|x|
        
    * 사용하는 데이터 구조와 언어, 구현의 방법, 환경의 차이에 따라사 최적의 알고리즘이 존재한다. 

    * merge의 경우 단일 링크드 리스트와 궁합이 맞다.

    * heap sort는 이중 링크드 리스트와 어울린다. 이런거 한번 따져 보자. 재미있겠다.

    * GNU의 glibc의 sort는 값의 개수가 적을 때에는 Insertion sort를 사용하고, 메모리의 상황에 따라서 Merge sort나 Quick sort를 선택해서 사용한다. wow

    

