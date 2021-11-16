#ifndef SPECTRALSAMPLEDATA_H
#define SPECTRALSAMPLEDATA_H

#include <vector>
#include <memory>
#include <map>
#include <WCECommon.hpp>

namespace FenestrationCommon
{
    class CSeries;
}

namespace SpectralAveraging
{
    ///////////////////////////////////////////////////////////////////////////
    /// MeasuredRow
    ///////////////////////////////////////////////////////////////////////////
    struct MeasuredRow
    {
        MeasuredRow(double wl, double t, double rf, double rb);
        double wavelength;
        double T;
        double Rf;
        double Rb;
    };

    ///////////////////////////////////////////////////////////////////////////
    /// SampleData
    ///////////////////////////////////////////////////////////////////////////
    class SampleData
    {
    public:
        SampleData();
        virtual ~SampleData() = default;

        virtual void interpolate(const std::vector<double> & t_Wavelengths) = 0;
        virtual FenestrationCommon::CSeries & properties(FenestrationCommon::Property prop,
                                                         FenestrationCommon::Side side) = 0;

        virtual void cutExtraData(double minLambda, double maxLambda) = 0;

        virtual bool Flipped() const final;
        virtual void Filpped(bool t_Flipped) final;

    protected:
        bool m_Flipped;
    };

    ///////////////////////////////////////////////////////////////////////////
    /// CSpectralSampleData
    ///////////////////////////////////////////////////////////////////////////
    // Measured sample data for given wavelengths.
    class CSpectralSampleData : public SampleData
    {
    public:
        virtual ~CSpectralSampleData() = default;
        CSpectralSampleData();
        CSpectralSampleData(const std::vector<MeasuredRow> & tValues);

        static std::shared_ptr<CSpectralSampleData>
          create(const std::vector<MeasuredRow> & tValues);

        static std::shared_ptr<CSpectralSampleData> create();

        void addRecord(double t_Wavelength,
                       double t_Transmittance,
                       double t_ReflectanceFront,
                       double t_ReflectanceBack);

        virtual FenestrationCommon::CSeries & properties(FenestrationCommon::Property prop,
                                                         FenestrationCommon::Side side) override;

        virtual std::vector<double> getWavelengths() const;
        virtual void interpolate(std::vector<double> const & t_Wavelengths) override;

        virtual void cutExtraData(double minLambda, double maxLambda) override;

    protected:
        virtual void calculateProperties();
        void reset();

        std::map<std::pair<FenestrationCommon::Property, FenestrationCommon::Side>,
                 FenestrationCommon::CSeries>
          m_Property;

        bool m_absCalculated;
    };

    ///////////////////////////////////////////////////////////////////////////
    /// PVM
    ///////////////////////////////////////////////////////////////////////////
    enum class PVM
    {
        EQE,
        VOC,
        FF
    };

    class EnumPVM : public FenestrationCommon::Enum<PVM>
    {};

    inline EnumPVM::Iterator begin(EnumPVM)
    {
        return EnumPVM::Iterator(static_cast<int>(PVM::EQE));
    }

    inline EnumPVM::Iterator end(EnumPVM)
    {
        return EnumPVM::Iterator(static_cast<int>(PVM::FF) + 1);
    }

    ///////////////////////////////////////////////////////////////////////////
    /// PVMeasurement
    ///////////////////////////////////////////////////////////////////////////
    struct PVMeasurement
    {
        PVMeasurement(double eqe, double voc, double ff);

        double EQE;
        double VOC;
        double FF;
    };

    ///////////////////////////////////////////////////////////////////////////
    /// PVMeasurementRow
    ///////////////////////////////////////////////////////////////////////////
    struct PVMeasurementRow
    {
        PVMeasurementRow(double wavelength,
                         const PVMeasurement & front,
                         const PVMeasurement & back);

        double wavelength;
        PVMeasurement front;
        PVMeasurement back;
    };

    ///////////////////////////////////////////////////////////////////////////
    /// PhotovoltaicSampleData
    ///////////////////////////////////////////////////////////////////////////
    class PhotovoltaicSampleData : public CSpectralSampleData
    {
    public:
        PhotovoltaicSampleData(const CSpectralSampleData & spectralSampleData);
        PhotovoltaicSampleData(const CSpectralSampleData & spectralSampleData,
                               const std::vector<PVMeasurementRow> & pvMeasurements);

        void interpolate(const std::vector<double> & t_Wavelengths) override;

        void cutExtraData(double minLambda, double maxLambda) override;

        void addRecord(double m_Wavelength,
                       const PVMeasurement frontSide,
                       const PVMeasurement backSide);

        void addRecord(const PVMeasurementRow & pvRow);

        FenestrationCommon::CSeries pvProperty(const FenestrationCommon::Side side,
                                               const PVM prop) const;

    private:
        std::map<std::pair<FenestrationCommon::Side, PVM>, FenestrationCommon::CSeries> m_PVData;
    };

}   // namespace SpectralAveraging

#endif
