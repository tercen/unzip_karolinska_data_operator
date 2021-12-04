# Template R operator

The `Template R operator` is a template repository for the creation of R operators in Tercen. An overview of steps for developing an operator are:

1. create a github repo
2. install tercen_studio
3. login to tercen_studio
4. git clone the newly created repo
5. start developing in R in tercen_studio
6. add R packages to the repo
7. push to the github repo
8. go to tercen and install the operator

More information can be found in [Tercen app builder's guide](https://tercen.github.io/appbuilders-guide/).

Below is the operator README standard structure:

##### Description

The `Template R operator` is a template repository for the creation of R operators in Tercen.

##### Usage

Input projection|.
---|---
`x-axis`        | type, description 
`y-axis`        | type, description 
`row`           | type, description 
`column`        | type, description 
`colors`        | type, description 
`labels`        | type, description 

Input parameters|.
---|---
`input_var`        | parameter description

Output relations|.
---|---
`output_var`        | output relation
`Operator view`        | view of the Shiny application

##### Details

Details on the computation.

##### See Also

[template_shiny_operator](https://github.com/tercen/template_shiny_operator)
, [template_docker_operator](https://github.com/tercen/template_docker_operator)

