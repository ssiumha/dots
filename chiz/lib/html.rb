module Lib
  class HtmlChiz < Base
    desc 'input_validate', 'input tag validation'
    def input_validate
      puts <<~END
        # simply regex
        <input required pattern="[A-Za-z]{3}">

        # oninvalid
        #   https://developer.mozilla.org/en-US/docs/Web/API/HTMLInputElement/invalid_event
        #   https://developer.mozilla.org/en-US/docs/Web/API/HTMLInputElement/setCustomValidity
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

        # pure js
        function validate(fields) {
          const errors = {};
          if (fields.username.length < 6) {
            errros.username = 'username must be at least 6 char'
          }

          return errors;
        }
      END
    end
  end
end
