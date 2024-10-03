import React, { useState, useEffect } from 'react';
import GovernmentUI from './GovernmentUI';

const App = () => {
  const [government, setGovernment] = useState(null);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState(null);

  useEffect(() => {
    const initializeGovernment = async () => {
      try {
        setLoading(true);
        // First, try to ping the existing government
        let response = await fetch('http://localhost:8080/government/ping');
        if (!response.ok) {
          // If ping fails, create a new government
          response = await fetch('http://localhost:8080/government/create', { method: 'POST' });
        }
        if (!response.ok) {
          throw new Error(`HTTP error! status: ${response.status}`);
        }
        const data = await response.json();
        console.log("Received government data:", data); // Add this line for debugging
        setGovernment(data);
      } catch (error) {
        console.error('Failed to initialize government:', error);
        setError(error.message);
      } finally {
        setLoading(false);
      }
    };

    initializeGovernment();
  }, []);

  if (loading) {
    return <div>Loading government data...</div>;
  }

  if (error) {
    return <div>Error: {error}</div>;
  }

  return (
    <div style={{ fontFamily: 'Arial, sans-serif', maxWidth: '1200px', margin: '0 auto', padding: '20px' }}>
      <h1>Region Simulator</h1>
      {government ? (
        <GovernmentUI initialGovernment={government} />
      ) : (
        <div>No government data available.</div>
      )}
    </div>
  );
};

export default App;