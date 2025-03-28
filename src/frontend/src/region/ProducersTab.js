import React, { useState } from 'react';
import styles from './RegionModal.module.css';
import tabStyles from './ProducersTab.module.css';

const ProducerDetailItem = ({ label, value }) => (
  <div className={tabStyles.detailItem}>
    <span className={tabStyles.detailLabel}>{label}:</span>
    <span className={tabStyles.detailValue}>{value}</span>
  </div>
);

const BondsList = ({ bonds }) => {
  const [expandedBond, setExpandedBond] = useState(null);

  if (!bonds || Object.keys(bonds).length === 0) {
    return <div className={tabStyles.noBonds}>No outstanding bonds</div>;
  }

  return (
    <div className={tabStyles.bondsContainer}>
      <h5 className={tabStyles.bondsTitle}>Outstanding Bonds</h5>
      <div className={tabStyles.bondsList}>
        {Object.entries(bonds).map(([id, bond]) => (
          <div key={id} className={tabStyles.bondItem}>
            <div
              className={tabStyles.bondHeader}
              onClick={() => setExpandedBond(expandedBond === id ? null : id)}
            >
              <span className={tabStyles.bondId}>Bond: {id.slice(0, 8)}...</span>
              <span className={tabStyles.bondAmount}>${bond.totalOutstanding}</span>
              <span className={`${tabStyles.expandIcon} ${expandedBond === id ? tabStyles.expanded : ''}`}>
                {expandedBond === id ? '▼' : '▶'}
              </span>
            </div>

            {expandedBond === id && (
              <div className={tabStyles.bondDetails}>
                <ProducerDetailItem label="Principal" value={`$${bond.principal}`} />
                <ProducerDetailItem label="Interest Rate" value={`${bond.interestRate * 100}%`} />
                <ProducerDetailItem label="Total Outstanding" value={`$${bond.totalOutstanding}`} />
              </div>
            )}
          </div>
        ))}
      </div>
    </div>
  );
};

const ResourceList = ({ resources }) => {
  if (!resources || Object.keys(resources).length === 0) {
    return <div className={tabStyles.noResources}>No stored resources</div>;
  }

  return (
    <div className={tabStyles.resourcesContainer}>
      {Object.entries(resources).map(([name, amount]) => (
        <div key={name} className={tabStyles.resourceItem}>
          <span className={tabStyles.resourceName}>{name}:</span>
          <span className={tabStyles.resourceAmount}>{amount}</span>
        </div>
      ))}
    </div>
  );
};

const ProducerDetailsView = ({ producer }) => {
  if (!producer) return null;

  return (
    <div className={tabStyles.producerDetailsStructured}>
      <div className={tabStyles.detailsSection}>
        <ProducerDetailItem label="ID" value={producer.id} />
        <ProducerDetailItem label="Region ID" value={producer.regionId} />
        <ProducerDetailItem label="Resource Produced" value={producer.resourceProduced} />
        <ProducerDetailItem label="Workers" value={`${producer.workers} / ${producer.maxWorkers}`} />
        <ProducerDetailItem label="Wage" value={`${producer.wage}`} />
        <ProducerDetailItem label="Base Production" value={producer.baseProduction} />
      </div>

      <div className={tabStyles.resourcesSection}>
        <h5 className={tabStyles.subSectionTitle}>Stored Resources</h5>
        <ResourceList resources={producer.storedResources} />
      </div>

      <BondsList bonds={producer.outstandingBonds} />
    </div>
  );
};

const ProducersTab = ({ producers }) => {
  const [selectedProducer, setSelectedProducer] = useState(null);

  if (!producers) {
    return <div>No producers available</div>;
  }

  return (
    <div>
      <h3 className={styles.sectionTitle}>Producers</h3>
      <div className={styles.producersGrid}>
        {Object.entries(producers).map(([id, producer]) => (
          <div
            key={id}
            onClick={() => setSelectedProducer(producer)}
            className={`${styles.producerCard} ${selectedProducer?.id === id ? styles.selectedProducer : ''}`}
          >
            <div className={styles.producerTitle}>
              {producer.resourceProduced} Producer
            </div>
            <div className={styles.producerId}>
              ID: {id.slice(0, 8)}...
            </div>
          </div>
        ))}
      </div>

      {selectedProducer && (
        <div className={styles.producerDetails}>
          <h4 className={styles.producerDetailsTitle}>
            {selectedProducer.resourceProduced} Producer Details
          </h4>
          <ProducerDetailsView producer={selectedProducer} />
        </div>
      )}
    </div>
  );
};

export default ProducersTab;