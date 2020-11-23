# Домашнее задание — счётчик посещений

Дописать код сервера, чтобы по запросу `/counter` выводился текст `Количество посещений:` и число посещений.

Количество посещений должно увеличиваться на 1 при каждом запросе `/counter`.

## Материал для самостоятельного изучения

1.  `import Data.IORef`

    1.  [`IORef a`](https://hackage.haskell.org/package/base/docs/Data-IORef.html#t:IORef) — ссылка на изменяемую ячейку памяти заданного типа.

    2.  [`newIORef :: a -> IO (IORef a)`](https://hackage.haskell.org/package/base/docs/Data-IORef.html#v:newIORef) — IO-действие создания новой IORef.

    3.  [`atomicModifyIORef :: IORef a -> (a -> (a, b)) -> IO b`](https://hackage.haskell.org/package/base/docs/Data-IORef.html#v:atomicModifyIORef) — IO-действие чтения и изменения IORef по заданной функции.

2.  `import Web.Scotty`

    1.  [`liftAndCatchIO :: IO a -> ActionM a`](https://hackage.haskell.org/package/scotty/docs/Web-Scotty.html#v:liftAndCatchIO) — преобразование IO-действий в действия ActionM (где `ActionM = ActionT Text IO`)

## Как проверять

1.  `stack test 05-web --file-watch`

2.  `stack run -- server --help`

3.  Открыть соответствующий адрес в браузере.

## Если что-то не получается

Искать в интернете, спрашивать в чате, у преподавателя или в сообществе RuHaskell.
