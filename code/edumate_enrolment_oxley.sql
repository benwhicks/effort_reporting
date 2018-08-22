-- Data extraction template for Oxley College effort enrolment data

WITH student_classes AS
    (
    SELECT
        view_student_class_enrolment.student_id, 
        class.class AS CLASS_NAME, 
        course.code||'_'||class.identifier AS CLASS_CODE, 
        course.code AS COURSE_CODE,
        class.class_id,
        REPLACE(salutation.salutation,'Unspecified','') AS TEACHER_TITLE,
        contact.firstname AS TEACHER_FIRSTNAME,
        contact.surname AS TEACHER_SURNAME,contact.EMAIL_ADDRESS  AS TEACHER_EMAIL,
        ROW_NUMBER() OVER (PARTITION BY view_student_class_enrolment.student_id ORDER BY view_student_class_enrolment.end_date DESC, view_student_class_enrolment.start_date DESC) AS CLASS_NUM
    FROM view_student_class_enrolment
        INNER JOIN class_teacher ON class_teacher.class_id = view_student_class_enrolment.class_id
            AND class_teacher.is_primary = 1
        INNER JOIN teacher ON teacher.teacher_id = class_teacher.teacher_id
        INNER JOIN contact ON contact.contact_id = teacher.contact_id
        LEFT JOIN salutation ON salutation.salutation_id = contact.salutation_id
        LEFT JOIN class ON class.class_id = view_student_class_enrolment.class_id
        LEFT JOIN course ON course.course_id = view_student_class_enrolment.course_id
    WHERE DATE(current_date) BETWEEN view_student_class_enrolment.start_date AND view_student_class_enrolment.end_date
    )

SELECT distinct
     form_run,
     student_number,
     student.reg_number,
     firstname,
     surname,
     REPLACE(REPLACE(contact.gender_id, 2, 'Male'), 3, 'Female') AS gender,
     contact.email_address,
     class,
     course,
     -- starting tutor group 
     --roll.class ROLL_CLASS,
     -- end tutor group
     student_classes.teacher_title,
     student_classes.teacher_firstname,
     student_classes.teacher_surname,
     student_classes.teacher_email,
     course.code || '.' || class.identifier as "class code"
FROM
    table(edumate.get_enroled_students_form_run(current_date)) gesfr
    INNER JOIN student on gesfr.student_id = student.student_id
    INNER JOIN contact on student.contact_id = contact.contact_id
    INNER JOIN form_run on gesfr.form_run_id = form_run.form_run_id
    Inner Join form on form_run.FORM_ID = form.FORM_ID
    INNER JOIN student_classes on student_classes.student_id = student.student_id
    inner JOIN class_enrollment on gesfr.student_id = class_enrollment.student_id
    INNER JOIN class on student_classes.class_id = class.class_id
    INNER JOIN academic_year on academic_year.academic_year_id = class.academic_year_id
    INNER JOIN course on course.course_id = class.course_id
    -- starting tutor group joins
    --INNER JOIN view_student_class_enrolment roll on gesfr.student_id=roll.student_id AND roll.end_date !< current_date AND roll.class_type_id =2
    --INNER JOIN view_student_class_enrolment vsce on gesfr.student_id=vsce.student_id AND vsce.end_date !< current_date AND vsce.class_type_id !=2
    --INNER JOIN view_student_mail_carers vsmc on gesfr.student_id=vsmc.student_id
    --INNER JOIN contact c1 on vsmc.carer1_contact_id=c1.contact_id
    --INNER JOIN class_type on vsce.class_type_id=class_type.class_type_id
    --LEFT JOIN contact c2 on vsmc.carer2_contact_id=c2.contact_id
    -- end tutor group joins

WHERE
    current_date  between class_enrollment.start_date and class_enrollment.end_date
    and form.form_id IN (9,10,11,12,13,14,15,16) -- Not sure what this does? 
    -- Ignore the following class types
    and NOT (upper(class.class) LIKE '%DEBATING%' OR upper(class.class) LIKE '%BAND%')  
    and NOT (upper(class.class) LIKE '%ENSEMBLE%' OR upper(class.class) LIKE '%DUX%')
    and NOT (upper(class.class) LIKE '%STUDY%' OR upper(class.class) LIKE '%FENCING%')
    and NOT (upper(class.class) LIKE '%KAYAK%' OR upper(class.class) LIKE '%BIKING%')
    and NOT (upper(class.class) LIKE '%VOX%' OR upper(class.class) LIKE '%CHORALE%')
    and NOT (upper(class.class) LIKE '%EXTRA%')  -- for extra PE classes, etc
    and NOT (upper(class.class) LIKE '%TUTOR%' OR upper(class.class) LIKE '%BASKETBALL%')
    and NOT (upper(class.class) LIKE '%POLO%' OR upper(class.class) LIKE '%ASSEMBLY%')
    and NOT (upper(class.class) LIKE '%DISTANCE%' OR upper(class.class) LIKE '%WIDE%')
    and NOT (upper(class.class) LIKE '%PRODUCTION%')
    and NOT (upper(class.class) LIKE '%ISA %')
    and NOT (upper(class.class) LIKE '%FILM%')
    and NOT (upper(class.class) LIKE '%SENIOR%') -- for Senior Science course
      
    and student_classes.class_code = (course.code || '_' || class.identifier)
    and (   
            class.CLASS_TYPE_ID IN (1,2)
            or 
            ((class.COURSE_ID, form_run.FORM_RUN_ID)  not in 
                   (select rpc.COURSE_ID, rpf.FORM_RUN_ID from report_period rp 
                    join REPORT_PERIOD_COURSE rpc on rp.REPORT_PERIOD_ID = rpc.REPORT_PERIOD_ID
                    join REPORT_PERIOD_FORM_RUN rpf on rpf.REPORT_PERIOD_ID =rp.REPORT_PERIOD_ID
                    join ACADEMIC_YEAR ay on ay.ACADEMIC_YEAR_ID = rp.ACADEMIC_YEAR_ID
                    where year(  current_date ) =ay.ACADEMIC_YEAR                    
                   )
            )
            or upper(class.class) like '%STUDY%'
                        or upper(class.class) like '%LEARNING PLUS%'
        )
ORDER BY
    form_run, student_number, class
