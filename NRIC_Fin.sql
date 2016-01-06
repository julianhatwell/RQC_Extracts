SELECT DISTINCT ops.contactId AS contactId
, oah.applicationId
, COALESCE(CASE WHEN ops.fin = '' OR ops.fin IS NULL THEN NULL ELSE ops.fin END
	, CASE WHEN oah.FinNo = '' OR oah.FinNo IS NULL THEN NULL ELSE oah.FinNo END
	, ops.nric
	, 'No_IC') AS NRIC_FIN
FROM ods_profilestudent ops
INNER JOIN ods_admissionHistory oah
	ON ops.contactId = oah.contactId