<h1><b>Сейчас есть:</b></h1> 
Типы данных:
 - int;
 - str;
 - float;

Создать переменную можно так:
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


Для запуска нужно создать файл с исходным кодом, расширением evg
запуск пока работает так:
```bash
cargo run file_name.evg
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
