<h1>Для начала работы предпримите следующие шаги:</h1>
Необходимо установить Rust:

Linux/MacOS ввести в консоль:
```bash
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
``` 

Windows скачать и установить:
```http
https://win.rustup.rs
```

После успешной установки будет следующее сообщение:
```
Rust is installed now. Great!
```

Затем нужно склонировать репозиторий к себе, на локаль:
```bash
git clone https://github.com/EvgeniiAndronov/interpretator
```

Затем собрать проект и перенести его в PATH:
```bash
cd interpretator
cargo build --release
sudo cp target/release/lege /usr/local/bin/
```

Запуск скрипта:
```bash
lege main.evg
```

<h1>Синтаксис и возможности</h1>

Всего есть три типа данных, целые числа, строки, числа с плавающей точкой:
```bash
intValue: int;
strValue: str;
floatValue: float;
```

Присвоить значение можно так:
```bash
intValue = 5;
strValue = "Hello World!";
floatValue = 3.1415;
```

Операторы сложения, вычитания, деления, умножения:
```bash
intValue = ((val1 + val2) * 2) / 1 - 1;
floatValue = ((val1 + val2) * 2.0) / 3.0 - 1.0;
strValue = strVal1 + strVal2;

```

Вывести в консоль можно так:
```bash
consoleout(anyValue);
```
Условия, возможны множественные условия, блок else - не обязательный:
```bash
if (intValue > 5) {
  ...
} else {
  ...
}
#Или несколько условий, где && - И; || - или
if (intValue > 1 && floatValue > 1.0) {
  ...
} 

if (intValue > 1 || floatValue > 1.0) {
  ...
}
```

Цикл for имеет конструкцию:
```bash
for (i = 0; i < 10; i+1) {
  ...
}
# Можем так же менять "направление"
for (i = 10; i > 1; i-1) {
  ...
}
# Или менять шаг 
for (i = 1; i < 10; i+2) {
  ...
}
```

Работа с массивами:
```bash
array myArray[int, 5]; # Создание массива с типом int, размером 5 элементов, именем myArray  
myArray[0] = 1; # Значения присваиваются по индексу, нумерация с нуля
myValInt = myArray[0]; # В созданные переменные можно записывать значения элементов массива
# При создании массива, по дефолту пишутся значения в ячейки массива, для int = 0; float = 0.0; str = ""; 
```

Пример исполняемого файла, который выводит все четные целые от 0 до 5:
```bash
for (i = 0; i < 5; i + 1) {
    d: int;
    d = i / 2;
    if (d * 2 == i) {
        consoleout(i);
    }
}
```
Или такой пример, где считаем сумму цифр от 0 до 10:
```bash
res: int;
for (i = 0; i < 10; i+1) {
    res = res + i;
}
```

Вот пример поиска числа Фибоначи:
```bash
o: int;
o = 1;
l: int;
l = 0;
r: int;
r = 1;
num: int;
num = 35;

for (i = 0; i < num; i + 1) {
    l = o + r;
    o = r;
    r = l;
}

consoleout(l);
consoleout("Номер этого элемента");
consoleout(num + 2);
```

Пример работы с массивами
```bash
array myArray[int, 10];

for (i = 0; i < 10; i+1) {
  myArray[i] = i + 3;
}

if (myArray[0] > myArray[1]) {
  consoleout("[0] > [1]");
}
consoleout(myArray);
```
