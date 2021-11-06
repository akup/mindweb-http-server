# Mindweb http server

Это http framework для rest части похожий на [finatra](https://github.com/twitter/finatra), но кроме rest имеет sitemap из которого собирается полноценный сайт с автоматической ajax-навигацией и поддержкой roundtrip'ов. Работает на [netty](https://github.com/netty/netty), часть кода взята из [finagle](https://github.com/twitter/finagle). Для парсинга и сериализации json используется [Jckson](https://github.com/FasterXML/jackson), а для diff, merge и patch JSON'а [json-patch](https://github.com/java-json-tools/json-patch). Почему сейчас они в проекте, а не подтянуты через dependency в sbt, на данный момент мне не ясно :) Возможно, где-то что-то подправлялось.

Продукт существенно легче, чем и finatra и finagle. Есть вариант без использования nio (без netty), для embedded платформ. Но пока не выложен в открытый доступ.

Framework предствален здесь в простом варианте, когда сессия не хранится в отдельном микросервисе. Highload вариант есть, полностью основан на том, что в открытом доступе на этой страничке, но будет предоставляться исключительно на коммерческой основе [kuprin.alexander@gmail.com](по запросу)

В качестве шаблонизатора используется mustache. Который может отрабатывать как на сервере, так и на клиенте. Собственно в framework есть возможность подгружать только данные по ajax.
Для этих целей используется sitemap, который по вложенным кускам страницы (модулям) умеет отдавать только нужный модуль или всю страницу целиком, вся эта радость сопряжена с ajax навигацией по сайту.
Для того чтобы проанализировать странички, чтобы их собирать используется [https://github.com/lexborisov/Modest](modest), а именно css selector'ы. Сделано так, чтобы можно было менять sitemap и сами странички с разметкой, в какие места встраивать подстранички, и всё работало без перезапуска сервера.

Что не мало важно, в framework зашиты roundtrip'ы, то есть можно дёргать события на клиенте (в браузере) отправляя actor'у на сервере сообщение с параметрами. Можно отправить такое сообщение на все открытые страницы, а можно на конкретную открытую страницу или конкретную страницу, где было совершено какое-то действие и она находится в нужном состоянии. То есть на сервере может храниться состояние, каждой открытой страницы, это состояние может содержать не только параметры но и функции! и частичные (partial) функции тоже. GC запускается когда нет heartbeat от страницы. Можно и на клиенте дернуть функцию, которая вернёт promise, который будет заполняться при обработке событий на сервере вызыванных на обработчике этого запроса.
В highload вариации, когда мы не знаем на каком конкретно сервере обрабатыватся клиент (sticky session) можно отправлять сообщения сервису сессий по id пользователя, а дальше он сам уж разберётся, куда послать сообщение, чтобы оно отобразилось у клиенты в браузере (в идеале и на пуше в приложение).

Roundtrip'ы реализованы по технике comet, а не на WebSocket'ах. WebSocket стоит добавить.

Ничего не документировано :( Будет делаться здесь с примерами использования. Так как framework
* а) шустрый и легкий и может работать и на embedded платформах (к примеру на роутере под управлением openWRT)
* б) имеет удобную автоматическую ajax-навигацию с подгрузкой только данных (и запоминанием переходов в истории браузинга)
* в) очень асинхронный, имеет roundtrip'ы
* г) рассчитан на то, чтобы быть ближним к клиенту слоем highload решений (edge server)

Ближайшее время буду выкладывать тесты производительности по сравнению с другими framework'ами. Так как всё лишние поверх netty, захламляющее, удалено, подозреваю, что будет очень-очень близко к производительности netty.

Продукт имеет под собой годы разработки, которые заключались в том, чтобы сначала наворотить сложного, а потом всё это срезать до красивого и простого :)
