SELECT 
ocm.description
, ocm.moduleCode
, omi.modinstanceId
, COALESCE(oao.description, 'Full Time') AS CourseStructure
, ops.contactId
, ops.nationality
, ops.nric
, ops.fin
, ot.startDate
, ot.endDate
FROM ods_comodule ocm
INNER JOIN ods_status ocms
	ON ocm.statusId = ocms.statusId
	AND ocms.statusDesc = 'Active'
INNER JOIN ods_modinstance omi
	ON ocm.moduleId = omi.moduleId
INNER JOIN ods_enrollment oe
	ON omi.modinstanceId = oe.modinstanceId
INNER JOIN ods_profilestudent ops
	ON oe.contactId = ops.contactId
INNER JOIN ods_term ot
	ON omi.termId = ot.termId
LEFT OUTER JOIN ods_acadorg oao
	ON ot.orgId = oao.orgId
WHERE ocm.bridgingModule = '1'






