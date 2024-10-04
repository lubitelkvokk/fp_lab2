lab2
=====

An OTP library

Build
-----

    $ rebar3 compile

```
Test value:
T1 = lab2:insert(10, 10, lab2:empty()).
T2 = lab2:insert(5, 5, T1).    
T3 = lab2:insert(6, 6, T2).
T4 = lab2:insert(3, 3, T3).
T5 = lab2:insert(4, 4, T4).
T6 = lab2:insert(2, 2, T5).
T7 = lab2:insert(1, 1, T6).
```

```
T1 = lab2:insert_and_balance(1, 1, lab2:empty()).
T2 = lab2:insert_and_balance(3, 3, T1).
T3 = lab2:insert_and_balance(10, 10, T2).
T4 = lab2:insert_and_balance(5, 5, T3).
T5 = lab2:insert_and_balance(7, 7, T4).
T6 = lab2:insert_and_balance(9, 9, T5).
T7 = lab2:insert_and_balance(11, 11, T6).
T8 = lab2:insert_and_balance(100, 100, T7).
T9 = lab2:insert_and_balance(50, 50, T8).
```
Ребалансировка деревьев осуществляется при помощи специальных механизмов — методов вращения. Вращения бывают двух видов: левое и правое.

Вращение вправо выполняется за три шага:

- Текущий корень поддерева (D) заменяется на левый дочерний узел (B)

- Предыдущий корень (D) становится правым дочерним узлом для (B)

- Предыдущее правое поддерево узла (B) становится левым поддеревом для (D)

![alt text](resources/image1.png)

Вращение влево выполняется аналогично:

- Текущий корень поддерева (D) заменяется на правый дочерний узел ©

- Предыдущий корень (D) становится левым дочерним узлом для ©

- Предыдущее левое поддерево узла © становится правым поддеревом для (D)

![alt text](resources/image2.png)


Функции:

- добавление и удаление элементов;
- фильтрация;
- отображение (map);
- свертки (левая и правая);
    ``` erl
    create_tree_from_list_foldl(List) ->
        lists:foldl(fun insert/2, empty(), List).
    
    create_tree_from_list_foldr(List) ->
        lists:foldr(fun insert/2, empty(), List).
    ```
- структура должна быть моноидом.
