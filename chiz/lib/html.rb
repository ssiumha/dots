module Lib
  class HtmlChiz < Base
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
  end
end
