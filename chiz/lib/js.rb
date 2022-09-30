module Lib
  class JsChiz < Base
    md :require, 'nodejs module system', <<~MD
      commonJS의 모듈 시스템. 브라우저는 작동하지 않는다

      ```js
      module.export = function() {}
      ```
    MD

    md :import, 'statement, method', <<~MD
      https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/import
      https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/import

      import() -> dynamic import. ECMAScript module을 비동기적으로, 비모듈 환경에서 직접 로딩한다

      dynamic import는 Promise를 리턴하며, `import * as A from 'moduleName'`에서 A 값을 받아온다

      ```js
      import('ui')'.then(({ SubmitButton }) => { ... })

      const { SubmitButton } = import('ui')
      ```
    MD

    md :export, 'statement export', <<~MD
      ```js
      # ui.js
      export function Button() { ... }
      export const Button2() { ... }
      export class Button3() { ... }

      # 모아서 export도 가능
      # export { Button, Button2, Button3 }

      # main.js
      import { Button, Button2 } from 'ui'
      ```

      - `default export`는 코드내 명명이 통일되지 않을 수 있으니 사용안하는 편이 좋다
    MD

    md :fetch, 'async request fetch method', <<~MD, lang: :js
      fetch('/users', {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json'
        },
        body: JSON.stringify({
          type: type,
        })
      }).then(async res => console.log(await res.text()))
    MD

    md :url, 'window.location, URLSearchParams', <<~MD, lang: :js
      // https://javascript.info/url

      window.location.search //=> ?arg=1&test=1
      window.location.hash //=> url에서 # 뒤에 있는 문자만
      window.location.pathname //=> 마지막 path 구분자
      window.location.host //=> port 포함 host 주소만
      window.lcoation.hostname //=> port 제외
      window.location.origin //=> path 제외한 http가 포함된 주소
      window.location.href //=> http 포함된 fullpath

      var params = new URLSearchParams(window.location.search);
      params.set('page', 3);
      params.get('page');
      params.toString();
      object.fromEntries(params); //=> to hash

      var url = new URL(window.location.href);
      url.searchParams //=> new URLSearchParams(window.location.search) 랑 동일
      url.pathname = '/test'
      url.toString()
    MD

    md :event, 'stopPropagation, preventDefault, keycode', <<~MD
      event.target
        - 실제 이벤트가 발생한 요소.
          가령 onclick 이벤트 요소 안의 div 요소가 클릭되었다면 클릭된 실제 div가 반환된다

      event.currentTarget
        - 이벤트가 등록된 요소
          target과 다르게 실제 onclick이 등록된 요소가 반환된다

      event.stopPropagation
        - 현재 element에서 부모로 이벤트가 전파 (bubble up, bubbling) 되는걸 멈춘다

      event.preventDefault
        - a, button, input 등 html에 정의된 기본 동작을 막는다

      event.keyCode (deprecated)
        - == 13 -> enter

      event.key
        - 입력키를 문자열 형태로 반환. IE/Edge는 약간씩 달라서 주의 필요
        - '1', 'a', 'A', 'Enter', 'Backspace', 'Shift'

      event.code
        - 실제로 입력된 키를 기준으로 반환된다. ex) [a, A] => KeyA, [1, !] => Digit1
        - 'Digit1', 'KeyA', 'KeyA', 'Enter', 'Backspace', 'ShiftRight'
    MD

    md :query_selector, 'document query selector', <<~MD, lang: :jsx
      querySelectorAll(...).forEach(e => ...)

      // map을 쓰고 싶을 경우 Array로 감싸야한다
      Array.from(document.querySelectorAll(...)).map(e => 1)
    MD

    md :attribute, 'html dom attribute access cheatsheet', <<~MD, lang: :jsx
      // basic
      el.id

      // class
      el.classList.add('popup')
      el.classList.remove('popup')
      el.classList.contains('visible')
      el.classList.toggle('opened', true|false)

      // attribute (return text)
      el.getAttribute('data-user-id')

      // data
      el.dataset.userId  // 코드 검색할 때 `data-xxx` 형태로 찾기 힘든 단점이 있다. attr을 쓰는게 좋을듯

      // input
      <input form="post_form" value="<name>" type="text">
      el.type
      el.value
      el.name
      el.disabled = true|false
      el.form.submit()

      // text (대입하면 전체가 대체된다)
      el.textContent // tag를 제외한 plain한 텍스트를 전부 취합해서 출력
      el.innerHTML   // children의 tag를 포함한 HTML 텍스트
      el.outerHTML   // innerHTML + 자기자신 포함

      // style
      el.style.backgroundColor = 'red'
      el.style.backgroundColor = 'red'
    MD

    md :create_element, 'create element in js', <<~MD, lang: :jsx
      const im = document.createElement('script');
      im.type = 'importmap';
      im.textContent = JSON.stringify(importMap);
      document.currentScript.after(im);
    MD

    md :intl, 'date time intl format', <<~MD, lang: :jsx
      const relEn = new Intl.RelativeTimeFormat('en', { style: 'long', numeric: 'auto' });
      relEn.format(-1, 'day'); // 'yesterday'
      relEn.format(0, 'year'); // 'this year'
      relEn.format(2, 'month'); // 'in 2 months'
    MD
  end
end
