# Домашнее задание

На все решения должны быть написаны тесты.

1.  (5 баллов) В таблице результатов выводить имя в формате

        first_name last_name

    Например,

        Jerzy Syrowiecki

    Если фамилии нет, не ставить лишний пробел.

2.  (5 баллов) Принимать ставку из поля `value` объекта [`Dice`](https://core.telegram.org/bots/api#dice).

## Материал для самостоятельного изучения

1.  [`readMaybe`](https://hackage.haskell.org/package/base/docs/Text-Read.html#v:readMaybe) — прочитать число из строки с результатом в `Maybe`. Для сравнения: `read` в случае ошибки бросает исключение.
2.  [`isPrefixOf`](https://hackage.haskell.org/package/text/docs/Data-Text.html#v:isPrefixOf) — проверить, что строка текста начинается с указанного префикса.
3.  [`stripPrefix`](https://hackage.haskell.org/package/text/docs/Data-Text.html#v:stripPrefix) — проверить, что строка текста начинается с указанного префикса, и отрезать его.

## Как проверять

Локально, без запуска бота:

    .test/stack-test 07-json --file-watch

Бота запускать необязательно, но если хочется, то

    stack run bot

Токен для [@quutar_bot](https://t.me/quutar_bot) можно спросить у [преподавателя](https://t.me/cblp_su). Токен коммитить нельзя.

Или можете завести своего бота.

## Если что-то не получается

Искать в интернете, спрашивать в чате, у преподавателя или в сообществе RuHaskell.
