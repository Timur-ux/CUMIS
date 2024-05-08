# CUMIS 1.0
Самопальный компилируемый импетаривный ЯП КУМЫС (C/C++ like)

## КУМЫС:
- Коммунистический
- Универсальный
- Многофункциональный
- бЫстрый
- Саморазвивающийся
## Компилятор написан на F#

## Синтаксис

### Базовые типы
| тип | обозначение |
| --- | ----------- |
| Целый | Цел |
| Дробный | Дроб |
| Булевский | Булка |
| Строка | Строка |

### Функции 

Объявление функции происходит следующим образом:
```
Пятилетка <имя функции>(<Тип> <Аргумент1>, <Тип> <Аргумент2>, ...) {
    <Тело функции>
    Сдать <возвращаемое значение>;
}
```

В случае отсутствия возвращаемого значения возвращается целочисленный 0
### Операторы
Полный список поддерживаемых операторов:
| Оператор | Оператор | Оператор | Оператор |
| -------- | -------- | -------- | -------- |
|     +    |     -    |     *    |     /    |
|     =    |     ==   |     !=   |     <    |
|     >    |     >=   |     <=   |          |

Семантика операторов аналогична оной в СИ

### Приведение типов
Отсутствует. Может потом добавлю
## Пример программы
fact.CUM

```
Пятилетка факториал(Цел н) {
    Если(н <= 1) {
        Сдать 1;
    }
    Сдать н * факториал(н - 1);
}

Пятилетка СлаваКПСС() {
    Печать("Госзаказ = ", факториал(5));
    Сдать 0;
}

Цел количествоСлавы = СлаваКПСС();
Печать("Количество славы:", количествоСлавы, "!");
```

## Описание компилятора
Компиляция происходит в 2 этапа
### 1. Построение AST дерева программы
Для этого используется библиотека FParsec. Для каждой конструкции создан свой парсер. Потом они объединяются в единый парсер, который пытается применить каждый из них по порядку до первого успеха.

К данному парсеру применяется комбинатор many, который применяет описанный выше парсер до тех пор, пока может и все результаты объединяет в список. Получается список из так называемых *утверждений* (statements), каждое из оных описывает оператор присваивания, цикл, условный оператор и т.д.

- Структуру AST дерева можно посмотреть в файле ./src/AST/AST.fs
- Парсер базовых типов описан в файле ./src/BaseParsers/Library.fs
- Парсер выражений описан в файле ./src/expr_p/expr_p.fs
- Главный парсер AST дерева описан в файле ./src/parser/parser.fs

### 2. Вычисление AST дерева
Собственно само выполнение скомпилированной программы.

Главная функция этого модуля:
```f#
let rec eval_AST (ast: list<AST.Statement>) = ...
```
Принимает на вход список из утверждений и последовательно их выполняет.

Вычисление *выражений*(арифметические выражения и вызовы функций) вынесены в отдельную функцию
```f#
let rec eval_expr (expr: AST.Expr) = ...
```

Для хранения таблицы имен используется пара из двух Map
1. Хранит пары <идентификатор, адрес>
2. Хранит пары <адрес, данные>

Этот способ хранения является более гибким, чем хранение пар <идентификатор, данные>, т.к. например, в рекурсивных функциях, промежуточные аргументы придется хранить в замыканиях, что не является хорошим методом с моей точки зрения, т.к. изнутри программы они не будут доступны.

Также использования адресов позволит впоследствии легко добавить поддержку указателей

- Описание вычисляющего модуля можно посмотреть в файле ./src/evaluator/evaluator.fs
- Описание модуля для работы с адресами можно посмотреть в файле ./src/malloc/Library.fs
- Описание функции для вычисление бинарного оператора можно в файле ./src/funcOf/Library.fs
- Описание *системных* функций(Печать(), например) можно в файле ./src/sysCalls/Library.fs

### 3. Логгирование
Сделал небольшой логгер, который при каждой компиляции записывает в лог полученное в этапе парсинга AST-дерево, а также все утверждения, которые вычисляющий модуль пытается применить на этапе вычисления программы. Это может помочь с выявлением багов и наблюдением за работой компилятора

## Планы на будущее
- Добавить поддержку структур(Компартия)
- Добавить псевдо адресную нотацию, т.е. поддержку указателей

## Компиляция компилятора
Для написания компилятора использовался ЯП F# на платформе .NET 8.0

В Makefile есть цель build для компиляции и run для компилирования программы, записанной в файле ./testProgram.CUM

Если хотите скомпилировать свою программу, просто вызовите команду
```
./src/app/bin/Debug/net8.0/app <Имя файла.CUM>
```

