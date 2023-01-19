module Lib
  class HtmlChiz < Base
    md :form, 'cheatsheet form', <<~MD, lang: :jsx
      // form 바깥에서 form 지정하기
      <form id="myform">
        <input type="text" name="name"/>
      </form>

      <input type="submit" form="myform" value="Update"/>

      // formaction으로 한 form에서 여러 역할 하기
      <form id="myform">
        <input type="submit" formaction="/url1"/>
        <input type="submit" formaction="/url2" formmethod="post"/>
      </form>
    MD

    md :table, 'table snippets', <<~MD, lang: :jsx
      // thead tbody tfoot -> table 내부 구분
      // tr -> table row
      // th -> table header
      // td -> table data

      <table>
        <thead>
          <tr>
            <th>id</th>
            <th>title</th>
            <th>date</th>
          </tr>
        </thead>

        <tbody>
          <tr>
            <td>1</td>
            <td>Test</td>
            <td>2000-01-01</td>
          </tr>
          <tr>
            <td>2</td>
            <td>Test 2</td>
            <td>2000-01-01</td>
          </tr>
        </tbody>

        <tfoot>
          <tr>
            <td>count</td>
            <td>2</td>
          </tr>
        </tfoot>
      </table>
    MD

    md :email_validate, 'easy email validate', <<~MD, lang: :jsx
      // regex
      <input type="text" required pattern="[^@]+@[^@]+\\.[a-zA-Z]{2,6}"/>

      // built in
      <input type="email"/>

      const validateEmail = (email) => {
        const input = document.createElement('input');
        input.type = 'email';
        input.required = true;
        input.value = email;
        return input.checkValidity();
      }
    MD

    md :input_validate, 'input tag validation', <<~MD, lang: :jsx
      // simply regex
      <input required pattern="[A-Za-z]{3}"/>

      // oninvalid
      //   https://developer.mozilla.org/en-US/docs/Web/API/HTMLInputElement/invalid_event
      //   https://developer.mozilla.org/en-US/docs/Web/API/HTMLInputElement/setCustomValidity
      const nameInput = document.querySelector('input');
      nameInput.addEventListener('input', () => {
        nameInput.setCustomValidity('')
        nameInput.checkValidity()
      );
      nameInput.addEventListener('invalid', () => {
        if (nameInput.value === '') {
            nameInput.setCustomValidity('empty username');
        } else {
            nameInput.setCustomValidity('');
        }
      });

      // pure js
      function validate(fields) {
        const errors = {};
        if (fields.username.length < 6) {
          errros.username = 'username must be at least 6 char'
        }

        return errors;
      }
    MD

    md :insert_dom, 'create dom in js', <<~MD, lang: :js
      const elem = document.createElement('div');
      elem.id = '#master-remocon';
      elem.style.position = 'fixed';
      elem.style.bottom = '0';
      elem.style.right = '0';
      elem.style.backgroundColor = 'white';
      elem.style.width = '90px';
      elem.style.height = '90px';
      elem.style.zIndex = '99999';

      document.body.prepend(elem);

      ////

      var xmlString = "<div id='foo'><a href='#'>Link</a><span></span></div>";
      var doc = new DOMParser().parseFromString(xmlString, "text/xml");
      document.body.prepend(doc.firstChild);

      ////

      var wrapper= document.createElement('div');
      wrapper.innerHTML= '<div><a href="#"></a><span></span></div>';
      var div= wrapper.firstElementChild;

      ////

      var d1 = document.getElementById('one');
      d1.insertAdjacentHTML('afterend', '<div id="two">two</div>');

      ////

      const newList = document.createElement('div');
      newList.innerHTML = `<%= j render partial: 'writer_box', collection: @writers, as: :writer %>`;

      // remove duplicate writer-box
      const renderedWriters = Array.from(document.querySelectorAll('.writer-box')).map(e => e.dataset.writerId);
      Array.from(newList.childNodes).forEach(e => renderedWriters.includes(e.dataset.writerId) && e.remove());

      writerBoxList.innerHTML += newList.innerHTML;
    MD

    md :dialog, 'popup, no z-index', <<~MD, lang: :jsx
      // z-index, 부모 영향없이 온전한 popup을 띄울 수 있다
      // - 나오는 순서대로 스태킹 된다
      // ::backdrop으로 뒷면 제어 가능
      // HTMLDialogElement는 show, showDialog, close 함수와 open boolean 변수를 지원한다
      //  - showDialog는 온전히 modal popup 처럼 동작하게 된다
      <dialog open onclose="console.log('closed!')">
        <form method="dialog">
          <menu>
            <button value="default">확인</button>
          </menu>
        </form>
      </dialog>

      <menu>
        <button onclick="document.querySelector('dialog').showModal()">
          팝업 열기
        </button>
      </menu>

      // 모달 뜨면 스크롤 막기
      html:has(dialog[open]) {
        overflow: hidden;
      }

      // open-ui에서도 정의중..
      //   https://open-ui.org/components/popup.research.explainer
      // 옵션은 auto, hint, manual
      // popuptoggletarget, popupshowtarget, popuphidetarget으로 다른 element에서 이벤트를 트리거시킬 수 있다
      // js에서 showPopUp(), hidePopUp() 함수와 :open selector로 컨트롤 할 수 있다
      // defaultopen: 페이지 로드시 자동으로 열림
      <div id="foo" popup="auto" defaultopen></div>

      <button popuptoggletarget="foo">open popup</button>
    MD
  end
end
