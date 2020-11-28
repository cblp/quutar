# Домашнее задание

При прохождении зазора добавлять случайный зазор у правого края.

Высота зазора — 200.

## Материал для самостоятельного изучения

1.  [ГПСЧ](https://ru.wikipedia.org/wiki/Генератор_псевдослучайных_чисел)
2.  [newStdGen](https://hackage.haskell.org/package/random/docs/System-Random.html#v:newStdGen)
3.  [randomR](https://hackage.haskell.org/package/random/docs/System-Random.html#v:randomR)

## Как проверять

    .test/stack-test 08-game --file-watch

    stack run game

Если на Windows возникнут проблемы с OpenGL, спросить в чате. На сайте [Gloss](http://gloss.ouroborus.net/) есть известные проблемы и известные решения.

### Ошибка `unknown GLUT entry glutInit`

-   Linux — установить `freeglut`

-   Windows

    1. Скачать `glut32.dll` с https://developer.download.nvidia.com/cg/glut.html

    2. Если есть права администратора, положить в `\Windows\SysWOW64` (или в `\Windows\System32` на 32-битной ситеме).

## Если что-то не получается

Искать в интернете, спрашивать в чате, у преподавателя или в сообществе RuHaskell.
