<h1><b>Сейчас есть:</b></h1> 
Целый тип данных

Создать переменную целого типа можно так:
```bash
name_value: int;
```
Присвоить значение можно так:
```bash
name_value = 5;
```
Вывести в консоль можно так:
```bash
consoleout(name_value);
```
Ветвление стндартное без нескольких вложенностей:
```bash
if (name_val > 5) {
  ...
} else {
  ...
}
```
Для запуска нужно создать файл с исходным кодом, расширением evg
запуск пока работает так:
```bash
cargo run file_name.evg
```

Пример исполняемого файла:
```bash
val1: int;
val2: int;
val1 = 5;
val2 = 3;

consoleout(val1 + val2);

if (val1 > val2) {
    consoleout(val1);
} else {
    consoleout(val2);
}

val3 = (val1 + val2) * 2;
consoleout(val3);
```