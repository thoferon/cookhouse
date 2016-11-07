open Sharp.Router

let overview = empty
let project  = CVF.const "projects" ^// VF.var
let job      = CVCVF.const "projects" ^/ VCVF.var ^/ CVF.const "jobs" ^// VF.var
