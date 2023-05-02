module Lib
  class DenoChiz < Base
    md :cli, 'shell script, zx', <<~MD, lang: :js
      #!/usr/bin/env deno run -A
      import $ from "https://deno.land/x/dax/mod.ts";

      await Promise.all([
        $`sleep 1; echo 1`,
        $`sleep 2; echo 2`,
        $`sleep 3; echo 3`,
      });
    MD
  end
end
