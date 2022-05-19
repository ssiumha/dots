module Lib
  class StimulusChiz < Base
    desc 'action', 'bind action cheatsheep'
    def action
      puts doc(<<~END)
        # event.preventDefault, event.stopPropagation shorthand
        <div data-controller="modal">
          <a data-action="modal#open:prevent"><a>

          <span data-action="click->modal#close:stop">Close</span>
        </div>

        <div data-controller="clipboard" data-action="click->cliboard#copy:prevent:stop"></div>
      END
    end
  end
end
